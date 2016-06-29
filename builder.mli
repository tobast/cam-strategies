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

(** {7 Transitive closure/reduction} *)

(** (aka. "transform the edges of the graph to make them match the `->'
    relation".) *)

(** Computes the transitive closure of the given dag, {e replacing} its edges.
    Complexity O(|V|^3). *)
val dag_transitiveClosure : dag -> unit

(** Computes the transitive reduction of the given dag, {e replacing} its
    edges. Complexity O(|V|^3). *)
val dag_transitiveReduction : dag -> unit

(********************************************)
(** {6 Games} *)

(** Thrown when trying to do operations relying on the tree parallel structure
    of the game, and when this structure doesn't match the expected one. *)
exception BadTreeStructure

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

(** Transforms a game (A || B) || C to a game A || (B || C)
    @raise BadTreeStructure when the game hasn't got the right shape. *)
val game_assocRight : game -> game

(** Same as {!game_assocRight}, but also returns the node mapping used. *)
val game_assocRight_mapped : game -> game * dagNode NodeMap.t

(** Transforms a game A || (B || C) to a game (A || B) || C
    @raise BadTreeStructure when the game hasn't got the right shape. *)
val game_assocLeft : game -> game

(** Same as {!game_assocLeft}, but also returns the node mapping used. *)
val game_assocLeft_mapped : game -> game * dagNode NodeMap.t

(** Tree structure whose leaves are labelled by identifiers. *)
type reassocTree = string Datatypes.binTreeStruct

(** Thrown by {!game_reassoc}. *)
exception BadReassocTree of reassocTree

(** Enforces associativity and commutativity (up to isomorphism) of games'
    parallel composition by remodelling the tree structure of the compositions
    of a game.
    
    [game_reassoc game fromTree toTree] tries to match [game] with the
    structure of [fromTree], whose leaves must bear unique identifiers,
    and tries to transform this structure into [toTree], whose leaves' labels
    must be a bijective mapping of those of [fromTree].
    
    The trees do not necessarily have to bear the whole tree structure of
    the parallel composition: a label can stand for a whole part of a tree.
    
    @raise BadTreeStructure if the first tree does not match the game.
    @raise BadReassocTree if one of the [reassocTree]s are badly labeled,
        or if the second tree's labels are not in bijective correspondence
        with the first tree's (and the raised tree is the second).
    *)
val game_reassoc : game -> reassocTree -> reassocTree -> game

(** Same as {!game_reassoc}, but also returns the node mapping used. *)
val game_reassoc_mapped : game -> reassocTree -> reassocTree ->
    game * dagNode NodeMap.t

(** Extracts the game [A] from [A || B].
    @raise BadTreeStructure if the game is not [A || B]. *)
val game_extractLeft : game -> game

(** Same as {!game_extractLeft}, but also returns the node mapping used. *)
val game_extractLeft_mapped : game -> game * dagNode NodeMap.t

(** Extracts the game [B] from [A || B].
    @raise BadTreeStructure if the game is not [A || B]. *)
val game_extractRight : game -> game

(** Same as {!game_extractRight}, but also returns the node mapping used. *)
val game_extractRight_mapped : game -> game * dagNode NodeMap.t

(*********************************************)
(** {6 Strategies} *)

(** Copies a strategy (creates new nodes). *)
val strat_copy : strategy -> strategy

(** The identity strategy on the given game. *)
val strat_id : game -> strategy

(** Creates a new empty strategy on the given game. *)
val strat_new : game -> strategy

(** Creates a new strategy on the given game, containing every event and
    edges of the game. Also returns a map of game nodes to strategy nodes. *)
val strat_newFilled_mapped : game -> strategy * dagNode NodeMap.t

(** Same as {!strat_newFilled_mapped}, but the map maps game node names to
    strategy nodes. *)
val strat_newFilled_namemapped : game -> strategy * dagNode SMap.t

(** Same as {!strat_newFilled_mapped}, without the map. *)
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

(** Transforms the underlying game with {!game_assocRight} and remaps
    correctly. *)
val strat_assocRight : strategy -> strategy

(** Transforms the underlying game with {!game_assocLeft} and remaps
    correctly. *)
val strat_assocLeft : strategy -> strategy

(** Transforms the underlying game with {!game_reassoc} and remaps
    correctly. *)
val strat_reassoc : strategy -> reassocTree -> reassocTree -> strategy

(** Transforms the underlying game with {!game_extractLeft} and remaps
    correctly. *)
val strat_extractLeft : strategy -> strategy

(** Transforms the underlying game with {!game_extractRight} and remaps
    correctly. *)
val strat_extractRight : strategy -> strategy

