
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
    
let formatNodeDotId parallel fmt = function CompId(comp,x) ->
    if Array.length parallel = 0 then
        Format.fprintf fmt "%d" x
    else
        Format.fprintf fmt "parallel%d_%d" comp x

let dotOfVert fmt labOverride parallel dagNode polarity =
    let label = if labOverride = "" then dagNode.nodeName else labOverride in
    let color = "color = \""^(match polarity with
        | PolNeg -> "red"
        | PolNeutral -> "black"
        | PolPos -> "green")^"\"" in
    if label = "" then (* Both its label and its override label are empty *)
        Format.fprintf fmt "%a [label=\"%a\", %s]@."
            (formatNodeDotId parallel) dagNode.nodeId
            (formatNodeId parallel) dagNode.nodeId
            color
    else
        Format.fprintf fmt "%a [label=\"%s (%a)\",%s]@."
            (formatNodeDotId parallel) dagNode.nodeId
            label
            (formatNodeId parallel) dagNode.nodeId
            color
            
let dotOfEdges fmt dotIdOf =
    List.iter (fun edge ->
        Format.fprintf fmt "%a -> %a@."
        dotIdOf edge.edgeSrc.nodeId
        dotIdOf edge.edgeDst.nodeId)
    
            
let dotOfStrategy fmt strat =
    let dotverts () =
        NodeSet.iter (fun nd -> dotOfVert fmt
                (getGameNode nd strat).nodeName
                [||] nd
                (getPolarity nd strat.st_strat)
            ) strat.st_strat.evts
    in
    
    let dotedges () =
        let dotIdOf = formatNodeDotId [||] in
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
            (getPolarity nd game.g_esp))
            game.g_esp.evts
    in
    let dotedges () =
        let dotIdOf = formatNodeDotId (game.g_parallel) in
        NodeSet.iter (fun nd -> dotOfEdges fmt dotIdOf nd.nodeOutEdges)
            game.g_esp.evts
    in
    
    Format.fprintf fmt "digraph {@.";
    dotverts ();
    dotedges ();
    Format.fprintf fmt "}@."
    
