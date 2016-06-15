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

(** Definition of all parallel composition operators (|||) *)

open Datatypes

module type S = sig
    (** [parallelGame a b] computes the game A | B *)
    val parallelGame : game -> game -> game
    
    (** Same as {!parallelGame}, also returning maps from the events of
     both games to the corresponding events in the parallel game. *)
    val parallelGame_mapped : game -> game ->
        game * (dagNode NodeMap.t) * (dagNode NodeMap.t)
    
    (** [parallelStrat a b] computes the strategy a | b *)
    val parallelStrat : strategy -> strategy -> strategy
end

module Canonical : S

