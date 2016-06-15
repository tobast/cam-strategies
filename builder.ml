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

let newNode name = {
        nodeId = Helpers.nextId () ;
        nodeName = name ;
        nodeInEdges = [] ;
        nodeOutEdges = []
    }

(******** ESP *************************************************************)

let esp_empty = { evts = NodeSet.empty ; pol = NodeMap.empty }

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
    
(******** Strategy ********************************************************)

let strat_new game = {
    st_strat = esp_empty ;
    st_game = game ;
    st_map = NodeMap.empty }

let strat_addEvent evt strat =
    (*TODO check that the node is part of this game *)
    let nNode = newNode "" in
    let nStrat = {
            evts = NodeSet.add nNode strat.st_strat.evts ;
            pol = NodeMap.add nNode
                (Helpers.getPolarity evt strat.st_game) strat.st_strat.pol
        } in
    nNode, { strat with
        st_strat = nStrat ;
        st_map = NodeMap.add nNode evt strat.st_map
    }

let strat_addEvents nEvts strat =
    List.fold_left (fun (cOut, cStrat) evt ->
        let nEvt,nStrat = strat_addEvent evt cStrat in
        nEvt :: cOut, nStrat) ([],strat) nEvts
        
let strat_addEdge = esp_addEdge

