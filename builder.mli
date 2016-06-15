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

(**
    A set of functions to build a game/strategy (events from list, add
    edges, ...)
*)

open Datatypes

(** {6 ESPs (aka. games)} *)

val esp_empty : esp

(** {7 Nodes} *)

(** [esp_addEvent pol esp] adds a new event of polarity [pol] to [esp],
 returning the created node and the new esp. *)
val esp_addEvent : polarity -> esp -> dagNode * esp

(** [esp_addNamedEvent name pol esp] adds a new event to [esp] with polarity
 [pol] named [name], returning the created node and the new esp. *)
val esp_addNamedEvent : string -> polarity -> esp -> dagNode * esp

(** Same as {!esp_addEvent}, but adds [n] events. *)
val esp_addEvents : int -> polarity -> esp -> dagNode list * esp

(** Same as {!esp_addNamedEvent}, but adds [n] elements. Returns an association
 list of the names to their node. *)
val esp_addNamedEvents : string list -> polarity -> esp ->
    (string*dagNode) list * esp

(** {7 Edges} *)

(** [esp_addEdge n1 n2] Adds an edge from [n1] to [n2]. *)
val esp_addEdge : dagNode -> dagNode -> unit

(** {6 Strategies} *)

(** Creates a new empty strategy on the given game. *)
val strat_new : game -> strategy

(** Adds a node from the game to the strategy. *)
val strat_addEvent : dagNode -> strategy -> dagNode * strategy

(** Adds a list of nodes from the game to the strategy. *)
val strat_addEvents : dagNode list -> strategy -> dagNode list * strategy

(** Adds a named node from the game to the strategy. Note that the name
 is purely for ease of use: it will never be used. *)
val strat_addNamedEvent : string -> dagNode -> strategy -> dagNode * strategy

(** Adds a list of named nodes from the game to the strategy.
 See {!strat_addNamedEvent}. *)
val strat_addNamedEvents : (string*dagNode) list -> strategy ->
    (string*dagNode) list * strategy

(** Adds an edge between two events of the strategy *)
val strat_addEdge : dagNode -> dagNode -> unit

