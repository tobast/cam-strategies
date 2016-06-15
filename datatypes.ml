
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

(** Defines the data types that will be used to represent strategies. *)

module SMap = Map.Make(String)

(** {6 DAG datatypes } *)

(** Unique node identifier session-wide (ie. two nodes, even from different
 DAGs, will not have the same nodeId). *)
type nodeId = int

(** Node in a DAG. The name is only used for display purposes. *)
type dagNode = {
    nodeId : nodeId ;
    nodeName : string ;
    mutable nodeOutEdges : dagEdge list ;
    mutable nodeInEdges : dagEdge list
}

and dagEdge = {
    edgeSrc : dagNode ;
    edgeDst : dagNode
}

module DagNode = struct
    type t = dagNode
    let compare a b =
        Pervasives.compare a.nodeId b.nodeId
end

module NodeSet = Set.Make(DagNode)
module NodeMap = Map.Make(DagNode)

type dag = NodeSet.t

(** {6 Games, strategies} *)

(** Polarity of an event. *)
type polarity = PolNeg | PolNeutral | PolPos

type esp = {
    evts : dag ;
    pol : polarity NodeMap.t
}

type game = esp

(** Raised when a function is applied to an ill-formed strategy
 (eg., the map is not a total map) *)
exception InvalidStrategy

type strategy = {
    st_strat : esp ;
    st_game : game ;
    
    (** Maps a node of the strategy to a node of the game *)
    st_map : dagNode NodeMap.t
}

