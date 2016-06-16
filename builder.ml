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

let mappedNode ndMap nd =
    (try NodeMap.find nd ndMap
    with Not_found -> nd)
let mapEdgeNodes ndMap edge =
    {
        edgeSrc = mappedNode ndMap edge.edgeSrc ;
        edgeDst = mappedNode ndMap edge.edgeDst
    }
let remapNode ndMap nd =
    nd.nodeInEdges <- List.map (mapEdgeNodes ndMap) nd.nodeInEdges ;
    nd.nodeOutEdges <- List.map (mapEdgeNodes ndMap) nd.nodeOutEdges

(** Copies a node. NOTE: the edges' other ends are left alike! *)
let copyNode nd =
    let nNd = newNode nd.nodeName in
    nNd.nodeInEdges <- nd.nodeInEdges ;
    nNd.nodeOutEdges <- nd.nodeOutEdges ;
    remapNode (NodeMap.singleton nd nNd) nNd ;
    nNd

(******** ESP *************************************************************)

let esp_empty = { evts = NodeSet.empty ; pol = NodeMap.empty }

let esp_copy_mapped esp =
    let remap = NodeSet.fold (fun nd cur ->
        NodeMap.add nd (copyNode nd) cur) esp.evts NodeMap.empty in
    NodeMap.iter (fun _ nNd -> remapNode remap nNd) remap ;
    let nEvts = NodeMap.fold (fun _ nNd cur -> NodeSet.add nNd cur)
        remap NodeSet.empty in
    let pol = NodeMap.fold (fun nd pol cur ->
        NodeMap.add (mappedNode remap nd) pol cur) esp.pol NodeMap.empty in
    { evts = nEvts; pol = pol }, remap
    
let esp_copy esp = fst @@ esp_copy_mapped esp

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
    Helpers.map2 List.rev Helpers.id @@
    List.fold_left (fun (cLst,cEsp) name ->
        let nNode,nEsp = esp_addNamedEvent name pol cEsp in
        ((name,nNode)::cLst),nEsp) ([],esp) names
        

let esp_addEdge n1 n2 =
    let edge = { edgeSrc = n1; edgeDst = n2 } in
    n1.nodeOutEdges <- edge::n1.nodeOutEdges ;
    n2.nodeInEdges <- edge::n2.nodeInEdges
    
(******** Strategy ********************************************************)
    
let strat_copy strat =
    let nStrat,stratMap = esp_copy_mapped strat.st_strat in
    let map = NodeMap.fold (fun fromNd toNd cur -> NodeMap.add
            (mappedNode stratMap fromNd) toNd cur)
        strat.st_map NodeMap.empty in
    {
        st_strat = nStrat ;
        st_game = strat.st_game ;
        st_map = map
    }

let strat_new game = {
    st_strat = esp_empty ;
    st_game = game ;
    st_map = NodeMap.empty }

let strat_addNamedEvent name evt strat =
    (*TODO check that the node is part of this game *)
    let nNode = newNode name in
    let nStrat = {
            evts = NodeSet.add nNode strat.st_strat.evts ;
            pol = NodeMap.add nNode
                (Helpers.getPolarity evt strat.st_game) strat.st_strat.pol
        } in
    nNode, { strat with
        st_strat = nStrat ;
        st_map = NodeMap.add nNode evt strat.st_map
    }
    
let strat_addEvent = strat_addNamedEvent ""

let strat_addNamedEvents nEvts strat =
    Helpers.map2 List.rev Helpers.id @@
    List.fold_left (fun (cOut, cStrat) (evtName,evt) ->
        let nEvt,nStrat = strat_addNamedEvent evtName evt cStrat in
        (evtName,nEvt) :: cOut, nStrat) ([],strat) nEvts
        
let strat_addEvents nEvts strat =
    let lst,nStrat = strat_addNamedEvents
        (List.map (fun x -> ("",x)) nEvts) strat in
    (List.map (fun (_,nd) -> nd) lst),nStrat
        
let strat_addEdge = esp_addEdge

