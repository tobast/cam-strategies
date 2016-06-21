
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
open Helpers

let formatNodeId parallel fmt = function CompId(comp,x) ->
    if Array.length parallel = 0 then
        Format.fprintf fmt "%d" x
    else
        Format.fprintf fmt "(%d,%d)" comp x
    
let formatNodeDotId prefix parallel fmt = function CompId(comp,x) ->
    if Array.length parallel = 0 then
        Format.fprintf fmt "%s%d" prefix x
    else
        Format.fprintf fmt "%sparallel%d_%d" prefix comp x

let stdFmtDotId = formatNodeDotId ""

let dotOfVert ?idFmt:(idFmt=stdFmtDotId) fmt labOverride
        parallel dagNode polarity attr =
    let label = if labOverride = "" then dagNode.nodeName else labOverride in
    let color = "color = \""^(match polarity with
        | PolNeg -> "red"
        | PolNeutral -> "black"
        | PolPos -> "green")^"\"" in
    if label = "" then (* Both its label and its override label are empty *)
        Format.fprintf fmt "%a [%s label=\"%a\", %s]@."
            (idFmt parallel) dagNode.nodeId
            attr
            (formatNodeId parallel) dagNode.nodeId
            color
    else
        Format.fprintf fmt "%a [%s label=\"%s (%a)\",%s]@."
            (idFmt parallel) dagNode.nodeId
            attr
            label
            (formatNodeId parallel) dagNode.nodeId
            color
            
let dotOfEdges ?attr:(attr="") fmt dotIdOf =
    List.iter (fun edge ->
        Format.fprintf fmt "%a -> %a [%s]@."
        dotIdOf edge.edgeSrc.nodeId
        dotIdOf edge.edgeDst.nodeId
        attr)
    
            
let dotOfStrategy fmt strat =
    let dotverts () =
        NodeSet.iter (fun nd -> dotOfVert fmt
                (getGameNode nd strat).nodeName
                [||] nd
                (getPolarity nd strat.st_strat) ""
            ) strat.st_strat.evts
    in
    
    let dotedges () =
        let dotIdOf = stdFmtDotId [||] in
        NodeSet.iter (fun nd -> dotOfEdges fmt dotIdOf nd.nodeOutEdges)
            strat.st_strat.evts
    in
    
    Format.fprintf fmt "digraph {@." ;
    dotverts () ;
    dotedges () ;
    Format.fprintf fmt "}@."
    
let dotOfGame fmt game =
    let dotverts () =
        NodeSet.iter (fun nd -> dotOfVert fmt
            nd.nodeName game.g_parallel nd
            (getPolarity nd game.g_esp) "")
            game.g_esp.evts
    in
    let dotedges () =
        let dotIdOf = stdFmtDotId (game.g_parallel) in
        NodeSet.iter (fun nd -> dotOfEdges fmt dotIdOf nd.nodeOutEdges)
            game.g_esp.evts
    in
    
    Format.fprintf fmt "digraph {@.";
    dotverts ();
    dotedges ();
    Format.fprintf fmt "}@."
    
let dotDebugOfStrategy fmt strat =
    (* Print the game as boxed nodes, the strategy as round nodes,
       the map between the two as blue dotted arrows.
       Everything game-related is dashed. *)
    let dotverts idPrefix attributes parallel getPol set =
        NodeSet.iter (fun nd -> dotOfVert fmt
                ~idFmt:(formatNodeDotId idPrefix)
                nd.nodeName parallel nd
                (getPol nd) attributes)
            set
    in
    let dotedges idPrefix attributes parallel set =
        let dotIdOf = formatNodeDotId idPrefix (parallel) in
        NodeSet.iter (fun nd -> dotOfEdges fmt dotIdOf
            nd.nodeOutEdges ~attr:attributes) set
    in
    let mapEdges dotIdFrom dotIdTo attributes map =
        NodeMap.iter (fun fromEvt toEvt ->
            Format.fprintf fmt "%a -> %a [%s]@."
            dotIdFrom (fromEvt.nodeId)
            dotIdTo (toEvt.nodeId)
            attributes) map
    in

    Format.fprintf fmt "digraph {@.";
    Format.fprintf fmt "subgraph cluster_game {@.";
    dotverts "gm" "style=dashed, shape=box," strat.st_game.g_parallel
        (fun nd -> NodeMap.find nd strat.st_game.g_esp.pol)
        strat.st_game.g_esp.evts ;
    dotedges "gm" "style=dashed" strat.st_game.g_parallel
        strat.st_game.g_esp.evts ;
        
    Format.fprintf fmt "} subgraph cluster_strat {@.";
    dotverts "st" "" [||] (fun x -> getPolarity x strat.st_strat)
        strat.st_strat.evts ;
    dotedges "st" "" [||] strat.st_strat.evts ;
   
    Format.fprintf fmt "}@.";

    mapEdges (formatNodeDotId "st" [||])
        (formatNodeDotId "gm" strat.st_game.g_parallel)
        "style=dotted, color=blue, arrowtail=tee, dir=both" strat.st_map ;
    
    Format.fprintf fmt "}@."

