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

(** General-purpose functions that can be called from everywhere. *)

open Datatypes

(** Raises {!Datatypes.InvalidStrategy} if not found. *)
val getPolarity : dagNode -> esp -> polarity

(** [getGameNode nd strat] returns the node mapped to [nd] in [strat.game],
 or raises {!Datatypes.InvalidStrategy} if not found. *)
val getGameNode : dagNode -> strategy -> dagNode

(** Returns the next available node id *)
val nextId : unit -> baseNodeId

(** [map2 f1 f2 (x,y)] returns [(f1 x, f2 y)] *)
val map2 : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd

(** Identity function. *)
val id : 'a -> 'a

exception MergeConflict
val mapMerger : 'b -> 'a option -> 'a option -> 'a option

(** Returns [true] iif the two ESPs are equal, without taking polarity into
 account. *)
val esp_eventsEquality : esp -> esp -> bool

(** Returns [true] iff the two games are equal. Doesn't mind if they do not
 agree on polarities. *)
val gamesEqualityNoPol : game -> game -> bool

