
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

let rec printWay fmt = function
| CompBase -> ()
| CompLeft(x) -> Format.fprintf fmt "←%a" printWay x
| CompRight(x) -> Format.fprintf fmt "→%a" printWay x

let rec printWayId fmt = function
| CompBase -> ()
| CompLeft(x) -> Format.fprintf fmt "l%a" printWayId x
| CompRight(x) -> Format.fprintf fmt "r%a" printWayId x

let formatNodeId fmt = function CompId(comp,x) ->
    if comp = CompBase then
        Format.fprintf fmt "(%d)" x
    else
        Format.fprintf fmt "(%a,%d)" printWay comp x

let formatNodeDotId prefix fmt = function CompId(comp,x) ->
    if comp = CompBase then
        Format.fprintf fmt "%s%d" prefix x
    else
        Format.fprintf fmt "%sparallel%a_%d" prefix printWayId comp x

let stdFmtDotId = formatNodeDotId ""

let dotOfVert ?idFmt:(idFmt=stdFmtDotId) ?dispId:(dispId=false) fmt labOverride
        dagNode polarity attr =
    let label = if labOverride = "" then dagNode.nodeName else labOverride in
    let color = (fun (x,y) -> Format.sprintf "color=\"%s\", class=\"%s\"" x y)
        (match polarity with
        | PolNeg -> "red","polneg"
        | PolNeutral -> "black","polneutral"
        | PolPos -> "green","polpos") in
    if label = "" then (* Both its label and its override label are empty *)
        Format.fprintf fmt "%a [%s label=\"%a\", %s]@."
            idFmt dagNode.nodeId
            attr
            formatNodeId dagNode.nodeId
            color
    else
        Format.fprintf fmt "%a [%s label=\"%s %a\",%s]@."
            idFmt dagNode.nodeId
            attr
            label
            (if dispId then formatNodeId else (fun x y -> ())) dagNode.nodeId
            color

let dotOfEdges ?attr:(attr="") fmt dotIdOf =
    List.iter (fun edge ->
        Format.fprintf fmt "%a -> %a [class=\"causality\"%s%s]@."
        dotIdOf edge.edgeSrc.nodeId
        dotIdOf edge.edgeDst.nodeId
        (if attr="" then "" else ", ") attr)


let dotOfStrategy fmt strat =
    let dotverts () =
        NodeSet.iter (fun nd -> dotOfVert ~dispId:false fmt
                (if nd.nodeName <> ""
                    then nd.nodeName
                    else (getGameNode nd strat).nodeName)
                nd (getPolarity nd strat.st_strat) ""
            ) strat.st_strat.evts
    in

    let dotedges () =
        let dotIdOf = stdFmtDotId in
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
            nd.nodeName nd
            (getPolarity nd game.g_esp) "")
            game.g_esp.evts
    in
    let dotedges () =
        let dotIdOf = stdFmtDotId in
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
    let dotverts dispId idPrefix attributes getPol set =
        NodeSet.iter (fun nd -> dotOfVert fmt
                ~idFmt:(formatNodeDotId idPrefix)
                ~dispId:dispId
                nd.nodeName nd
                (getPol nd) attributes)
            set
    in
    let dotedges idPrefix attributes set =
        let dotIdOf = formatNodeDotId idPrefix in
        NodeSet.iter (fun nd -> dotOfEdges fmt dotIdOf
            nd.nodeOutEdges ~attr:attributes) set
    in
    let mapEdges dotIdFrom dotIdTo attributes map =
        NodeMap.iter (fun fromEvt toEvt ->
            Format.fprintf fmt "%a -> %a [class=\"map\", %s]@."
            dotIdFrom (fromEvt.nodeId)
            dotIdTo (toEvt.nodeId)
            attributes) map
    in

    Format.fprintf fmt "digraph {@.";
    Format.fprintf fmt "subgraph cluster_game {@.";
    dotverts true "gm" "style=dashed, shape=box,"
        (fun nd -> NodeMap.find nd strat.st_game.g_esp.pol)
        strat.st_game.g_esp.evts ;
    dotedges "gm" "style=dashed" strat.st_game.g_esp.evts ;

    Format.fprintf fmt "} subgraph cluster_strat {@.";
    dotverts false "st" "" (fun x -> getPolarity x strat.st_strat)
        strat.st_strat.evts ;
    dotedges "st" "" strat.st_strat.evts ;

    Format.fprintf fmt "}@.";

    mapEdges (formatNodeDotId "st")
        (formatNodeDotId "gm")
        "style=dotted, color=blue, arrowtail=tee, dir=both" strat.st_map ;

    Format.fprintf fmt "}@."

let dispDot displayer x =
    let ch = Unix.open_process_out "dot -Tpng | feh -" in
    let fmt = Format.formatter_of_out_channel ch in
    displayer fmt x;
    Pervasives.close_out ch

let dispStrategy = dispDot dotOfStrategy
let dispGame = dispDot dotOfGame
let dispDebugStrategy = dispDot dotDebugOfStrategy

