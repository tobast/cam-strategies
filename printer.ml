
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

let dotOfVert fmt labOverride dagNode polarity =
    let label = if labOverride = "" then dagNode.nodeName else labOverride in
    let color = "color = \""^(match polarity with
        | PolNeg -> "red"
        | PolNeutral -> "black"
        | PolPos -> "green")^"\"" in
    if label = "" then (* Both its label and its override label are empty *)
        Format.fprintf fmt "%d [%s]@." dagNode.nodeId color
    else
        Format.fprintf fmt "%d [label=\"%s (%d)\",%s]@."
            dagNode.nodeId label dagNode.nodeId color
            
let dotOfStrategy fmt strat =
    let dotverts () =
        NodeSet.iter (fun nd -> dotOfVert fmt
                (getGameNode nd strat).nodeName nd
                (getPolarity nd strat.st_strat)
            ) strat.st_strat.evts
    in
    
    let dotedges () =
        NodeSet.iter (fun nd ->
            List.iter (fun edge ->
                Format.fprintf fmt "%d -> %d@."
                edge.edgeSrc.nodeId edge.edgeDst.nodeId)
            nd.nodeOutEdges) strat.st_strat.evts
    in
    
    Format.fprintf fmt "digraph {@." ;
    dotverts () ;
    dotedges () ;
    Format.fprintf fmt "}@."
    
    
