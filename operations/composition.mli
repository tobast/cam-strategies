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

(** Handles composition operations on strategies (with and without hiding). *)

open Datatypes

module type S = sig
    (** Raised if trying to compute the interaction of a game that is not
     composed (that is, not A || B). *)
    exception NotComposed of game
    exception GameNotFound
    
    (** Raised if trying to compute the interaction of [A||B] and [C||D]
    where [B <> C] (even when removing polarities). *)
    exception MismatchedGames
    
    (** [compInteraction s t] does the same as {!compInteractionOnGame},
        but determines automatically the game B such that it is the
        largest game verifying this property IGNORING POLARITIES. *)
    val compInteraction : strategy -> strategy -> strategy
    
    (** [compHidden s t] computes the composition with hiding of the
     intermediary states. See {!compInteraction}. *)
    val compHidden : strategy -> strategy -> strategy
end

module Canonical (Pullback : Pullback.S) (Parallel : Parallel.S) : S

