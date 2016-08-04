(*
 *  Strategies interpreter
 *
 *	This program is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Datatypes
open ModExtensions

module type S = sig
    exception NotComposed of game
    exception GameNotFound
    exception MismatchedGames
    val compInteraction : strategy -> strategy -> strategy
    val compHidden : strategy -> strategy -> strategy
end

module Canonical (Pullback : Pullback.S) (Parallel : Parallel.S) = struct
    exception NotComposed of game
    exception GameNotFound
    exception MismatchedGames

    let isComposite game =
        game.g_tree = None

    let compInteraction st1 st2 =
        (match st1.st_game.g_tree, st2.st_game.g_tree with
        | Some(TreeNode _), None
        | Some(TreeNode _), Some(TreeLeaf _) ->
                raise @@ NotComposed st2.st_game
        | None, _ | Some(TreeLeaf _), _ ->
                raise @@ NotComposed st1.st_game

        | Some (TreeNode(midTree2, rightTree)),
          Some (TreeNode(leftTree, midTree1)) ->
            if not @@ Helpers.treesEqualityNoPol midTree1 midTree2 then
                raise MismatchedGames;

            let leftGame = Builder.game_extractLeft st2.st_game
            and rightGame= Builder.game_extractRight st1.st_game in
            Pullback.pullback
                (Parallel.parallelStrat st2 (Builder.strat_id rightGame))
                (Builder.strat_assocLeft
                    (Parallel.parallelStrat (Builder.strat_id leftGame) st1))
        )

    let compHidden st1 st2 =
        let interact = compInteraction st1 st2 in
        Builder.dag_transitiveClosure interact.st_strat.evts ;
        (* Interact is (by construction) on a game of the form (A || B) || C,
         * we want to transform it to A || C *)

        let nTree = (match interact.st_game.g_tree with
            | None
            | Some (TreeLeaf _)
            | Some (TreeNode(TreeLeaf _, _)) ->
                raise (Builder.BadTreeStructure)
            | Some (TreeNode(TreeNode(l, _), r)) ->
                Some (TreeNode(l,r))) in

        let leftMap, leftEvts = NodeSet.fold (fun nd (curMap,curEvts) ->
            (match nd.nodeId with
            | CompId(CompLeft(CompLeft(comp)),id) ->
                let nNd = { nd with nodeId = CompId(CompLeft(comp),id) } in
                NodeMap.add nd nNd curMap, NodeSet.add nNd curEvts
            | _ -> curMap,curEvts))
            interact.st_game.g_esp.evts (NodeMap.empty, NodeSet.empty) in
        let rightMap, rightEvts = NodeSet.fold (fun nd (curMap,curEvts) ->
            (match nd.nodeId with
            | CompId(CompRight(comp),id) ->
                let nNd = { nd with nodeId = CompId(CompRight(comp),id) } in
                NodeMap.add nd nNd curMap, NodeSet.add nNd curEvts
            | _ -> curMap,curEvts))
            interact.st_game.g_esp.evts (NodeMap.empty, NodeSet.empty) in
        let globMap = NodeMap.merge Helpers.mapMerger leftMap rightMap in
        let remapPol map pol =
            NodeMap.fold (fun nd p cur -> (try
                    let nNd = NodeMap.find nd map in
                    NodeMap.add nNd p cur
                with Not_found -> cur)) pol NodeMap.empty in
        let nPols = NodeMap.merge Helpers.mapMerger
            (remapPol leftMap interact.st_game.g_esp.pol)
            (remapPol rightMap interact.st_game.g_esp.pol) in

        let remapEdges map nodes =
            let remapSingleEdge ed =
                {
                    edgeSrc = NodeMap.find ed.edgeSrc map ;
                    edgeDst = NodeMap.find ed.edgeDst map
                } in
            NodeSet.iter (fun nd ->
                nd.nodeInEdges <- List.map remapSingleEdge nd.nodeInEdges ;
                nd.nodeOutEdges <- List.map remapSingleEdge nd.nodeOutEdges ;
                ) nodes
        in
        remapEdges leftMap leftEvts ;
        remapEdges rightMap rightEvts ;

        let outGame = {
            g_esp = {
                evts = NodeSet.union leftEvts rightEvts ;
                pol = nPols };
            g_tree = nTree } in

        let stratMap = NodeMap.fold (fun sNd gNd cur -> (try
                let nNd = NodeMap.find gNd globMap in
                NodeMap.add sNd nNd cur
            with Not_found -> cur)) interact.st_map NodeMap.empty in

        let stratEvts = NodeSet.filter (fun nd -> NodeMap.mem nd stratMap)
            interact.st_strat.evts in

        let outStrat = {
            st_game = outGame ;
            st_map = stratMap ;
            st_strat = { interact.st_strat with
                evts = stratEvts }
            } in

        Builder.dag_transitiveReduction outStrat.st_strat.evts ;
        outStrat
end

