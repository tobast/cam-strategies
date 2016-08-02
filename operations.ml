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

module type S = sig
    val perp : game -> game
    val copycat : game -> strategy
    type copycatSide = CcLeft | CcRight
    val copycat_named : (dagNode -> copycatSide -> string) -> game -> strategy
    val (&&&) : strategy -> strategy -> strategy
    val (|||:) : game -> game -> game
    val (|||:~) : game -> game -> game * dagNode NodeMap.t * dagNode NodeMap.t
    val (|||) : strategy -> strategy -> strategy
    val (|||~) : strategy -> strategy ->
        strategy * dagNode NodeMap.t * dagNode NodeMap.t
    val ( *** ) : strategy -> strategy -> strategy
    val (@@@) : strategy -> strategy -> strategy
end

module Make
        (Pullback : Pullback.S)
        (Parallel : Parallel.S)
        (Compose : Composition.S)
= struct
    (** Warning! This implementation only modifies pol without copying
        anything. *)
    let rec perp game =
        let oppPol = function
        | PolPos -> PolNeg
        | PolNeg -> PolPos
        | PolNeutral -> PolNeutral in
        let nEsp = { game.g_esp
            with pol = NodeMap.map oppPol game.g_esp.pol } in
        let nTree =
            let rec chPol = function
            | TreeNode(l,r) -> TreeNode(chPol l, chPol r)
            | TreeLeaf(game) -> TreeLeaf(perp game) in
            (match game.g_tree with
            | None -> None
            | Some tree -> Some (chPol tree))
            in
        { g_esp = nEsp ; g_tree = nTree }

    let (&&&) = Pullback.pullback

    let (|||:) = Parallel.parallelGame
    let (|||:~) = Parallel.parallelGame_mapped
    let (|||) = Parallel.parallelStrat
    let (|||~) = Parallel.parallelStrat_mapped

    let ( *** ) = Compose.compInteraction
    let ( @@@ ) = Compose.compHidden

    exception ExnNeutral
    type copycatSide = CcLeft | CcRight

    let copycat_named namer game =
        let pGame = perp game in
        let nGame, leftMap, rightMap = pGame |||:~ game in
        let strat,stratMap = Builder.strat_newFilled_named_mapped nGame
            (fun nd -> match nd.nodeId with
            | CompId(CompLeft(_), _) -> namer nd CcLeft
            | CompId(CompRight(_),_) -> namer nd CcRight
            | CompId(CompBase,_) -> assert false)
            in
        NodeSet.iter (fun nd -> (try
            let fromMap,toMap = (match Helpers.getPolarity nd game.g_esp with
                | PolPos -> leftMap,rightMap
                | PolNeg -> rightMap,leftMap
                | PolNeutral -> raise ExnNeutral) in
            let fromNd = NodeMap.find (NodeMap.find nd fromMap) stratMap
            and toNd = NodeMap.find (NodeMap.find nd toMap) stratMap in

            Builder.strat_addEdge fromNd toNd

            with ExnNeutral -> ())) game.g_esp.evts ;
        strat

    let copycat game =
        copycat_named (fun nd _ -> nd.nodeName) game
end


module Canonical = Make
    (Pullback.Canonical)
    (Parallel.Canonical)
    (Composition.Canonical (Pullback.Canonical) (Parallel.Canonical))

