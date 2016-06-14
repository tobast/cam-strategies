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

let esp_empty = { evts = NodeSet.empty ; pol = NodeMap.empty }

let newNode name = {
        nodeId = Helpers.nextId () ;
        nodeName = name ;
        nodeInEdges = [] ;
        nodeOutEdges = []
    }

let esp_addNamedEvent name pol esp =
    let nNode = newNode name in
    nNode, {
        evts = NodeSet.add nNode esp.evts ;
        pol = NodeMap.add nNode pol esp.pol
    }
let esp_addEvent = esp_addNamedEvent ""

let esp_addEvents n pol esp =
    let rec doAdd lst esp = function
    | 0 -> lst,esp
    | n -> let nNode,nEsp = esp_addEvent pol esp in
        doAdd (nNode::lst) nEsp (n-1)
    in
    doAdd [] esp n

let esp_addNamedEvents names pol esp =
    List.fold_left (fun (cLst,cEsp) name ->
        let nNode,nEsp = esp_addNamedEvent name pol cEsp in
        ((name,nNode)::cLst),nEsp) ([],esp) names
        

let esp_addEdge n1 n2 =
    let edge = { edgeSrc = n1; edgeDst = n2 } in
    n1.nodeOutEdges <- edge::n1.nodeOutEdges ;
    n2.nodeInEdges <- edge::n2.nodeInEdges

