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

(** {6 ESPs} *)

val esp_empty : esp

(** Copies an ESP (creates new nodes), also returning a map
 of correspondance between the old and new nodes.
 
 WARNING: does not preserve {!Datatypes.CompId}s well! *)
val esp_copy_mapped : esp -> esp * dagNode NodeMap.t

(** Copies an ESP (creates new nodes).
 
 WARNING: does not preserve {!Datatypes.CompId}s well! *)
val esp_copy : esp -> esp

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

(********************************************)
(** {6 Games} *)

(** See {!esp_empty}. *)
val game_empty : game

(** Copies a parallel game A0 || ... || An to the same game,
 abstracted from its parallel composition. *)
val game_unparallelize : game -> game

(** Puts in parallel two games, returning maps from both games to the new
 game's events. Yes, it should belong to Operators, but it causes circular
 dependancies. *)
val game_parallel_mapped : game -> game ->
    game * dagNode NodeMap.t * dagNode NodeMap.t

(** See {!game_parallel_mapped}. *)
val game_parallel : game -> game -> game

(** See {!esp_copy_mapped}. Preserves parallel composition. *)
val game_copy_mapped : game -> game * dagNode NodeMap.t

(** See {!esp_copy}. Preserves parallel composition. *)
val game_copy : game -> game

(** See {!esp_addEvent}. *)
val game_addEvent : polarity -> game -> dagNode * game

(** See {!esp_addNamedEvent}. *)
val game_addNamedEvent : string -> polarity -> game -> dagNode * game

(** See {!esp_addEvents}. *)
val game_addEvents : int -> polarity -> game -> dagNode list * game

(** See {!esp_addNamedEvents}. *)
val game_addNamedEvents : string list -> polarity -> game ->
    (string*dagNode) list * game

(** See {!esp_addEdge}. *)
val game_addEdge : dagNode -> dagNode -> unit

(*********************************************)
(** {6 Strategies} *)

(** Copies a strategy (creates new nodes). *)
val strat_copy : strategy -> strategy

(** The identity strategy on the given game. *)
val strat_id : game -> strategy

(** Creates a new empty strategy on the given game. *)
val strat_new : game -> strategy

(** Creates a new strategy on the given game, containing every event and
    edges of the game. *)
val strat_newFilled : game -> strategy

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

