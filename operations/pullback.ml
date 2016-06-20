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

exception MismatchedGames
module type S = sig
    val pullback : strategy -> strategy -> strategy
end

module BottomUp = struct
    let invertedMapping strategy =
        let rev =
            NodeMap.fold (fun sNode gNode cur ->
                NodeMap.add gNode sNode cur) strategy.st_map
                NodeMap.empty in
        (*
        NodeSet.fold (fun evt cur ->
            if NodeMap.mem evt cur then cur
                else NodeMap.add evt None cur) strategy.st_game.evts rev
        *) rev

    let pullback s1 s2 =
        (* NOTE for now, everything is deterministic. Thus, each stragety
         * contains at most ONE copy of each event of the game.
         * This represetation WON'T WORK anymore when we'll plug
         * non-determinism in it.
         *)
        (* Copy: makes it possible to reuse the nodes directly on the pullback
         * and to use them on a map all together. *)
        
        if not @@ Helpers.gamesEqualityNoPol s1.st_game s2.st_game then
            raise MismatchedGames;
        
        (* Merge polarity inconsistencies as neutral *)
        let mergedPols = NodeMap.merge (fun _ x y -> match x,y with
            | None,None -> None
            | Some x, None | None, Some x -> Some x
            | Some x, Some y -> Some PolNeutral)
            s1.st_game.g_esp.pol s2.st_game.g_esp.pol in

        let game = { s1.st_game with g_esp = { s1.st_game.g_esp with
                pol = mergedPols
            } } in

        let cs1 = Builder.strat_copy s1
        and cs2 = (* Builder.strat_copy *) s2 in
        let eventsSet = NodeSet.union cs1.st_strat.evts cs2.st_strat.evts in
        
        let invmap1 = invertedMapping cs1
        and invmap2 = invertedMapping cs2 in
        
        (* Sets of dependancy: which events the given event depends on? *)
        let origDepSets = NodeSet.fold (fun evt cur ->
                NodeMap.add evt (List.fold_left (fun cSet cEdge ->
                    NodeSet.add cEdge.edgeSrc cSet)
                    NodeSet.empty evt.nodeInEdges) cur)
            eventsSet NodeMap.empty in
        
        let hasDeps dependancies invmap evt =
            (try
                let sEvt = NodeMap.find evt invmap in
                not @@ NodeSet.is_empty @@ NodeMap.find sEvt dependancies
            with Not_found -> true)
        in
        let hasAnyDeps depSets gEvt =
            hasDeps depSets invmap1 gEvt || hasDeps depSets invmap2 gEvt
        in
        
        let worklist = NodeSet.fold (fun gEvt cur ->
            if not @@ hasAnyDeps origDepSets gEvt
                then gEvt :: cur
                else cur) game.g_esp.evts [] in
        
        let rec remDeps strat (deps,newFree) nd = function
        | [] -> deps,newFree
        | hd::tl ->
            let cDeps = NodeMap.find hd deps in
            let nDeps = NodeSet.remove nd cDeps in
            let nFree =
                if NodeSet.is_empty nDeps && (not @@ NodeSet.is_empty cDeps)
                    then NodeSet.add (Helpers.getGameNode hd strat) newFree
                    else newFree in
            
            remDeps strat ((NodeMap.add hd nDeps deps),nFree) nd tl
        in

        let rec buildPullback pbNodesMap depSets cPb = function
        | [] -> cPb
        | cEvt::tl ->
            let sEv1 = NodeMap.find cEvt invmap1 in
            let sEv2 = NodeMap.find cEvt invmap2 in
            let name = if sEv1.nodeName = sEv2.nodeName
                then sEv1.nodeName
                else sEv1.nodeName ^","^sEv2.nodeName in
            let nNode, nPb = Builder.strat_addNamedEvent name cEvt cPb in 
            
            let evtDeps = NodeSet.fold (fun evt cur ->
                    NodeSet.add (NodeMap.find evt pbNodesMap) cur)
                (NodeSet.union
                    (NodeMap.find sEv1 origDepSets)
                    (NodeMap.find sEv2 origDepSets))
                NodeSet.empty
            in
            
            NodeSet.iter (fun evt -> Builder.strat_addEdge evt nNode) evtDeps;
            
            let nDepSets,nFree = remDeps cs2
                (remDeps cs1 (depSets,NodeSet.empty) sEv1
                    (List.map (fun x -> x.edgeDst) sEv1.nodeOutEdges))
                sEv2 (List.map (fun x -> x.edgeDst) sEv2.nodeOutEdges) in
            
            let nPbMap = NodeMap.add sEv1 nNode
                (NodeMap.add sEv2 nNode pbNodesMap) in
            
            let nWorklist = NodeSet.fold (fun elt curWL ->
                    if not @@ hasAnyDeps nDepSets elt
                        then elt :: curWL
                        else curWL) nFree tl in

            buildPullback nPbMap nDepSets nPb nWorklist
        in
        
        buildPullback NodeMap.empty origDepSets
            (Builder.strat_new game) worklist
end

module Canonical = BottomUp

