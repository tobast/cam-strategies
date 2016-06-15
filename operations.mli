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

(** Putting the operations together as a functor.
 The recommended operation mode is to {!Make} a module with the right
 functor parameters, or use a preset, and then to [open] it. *)

open Datatypes

module type S = sig
    (** Module created by {!Make}.

    Name conventions:
    - to be sure not to interfere with OCaml operators, if an operation is
    designated by the operator [x], its base operator will be [xxx].
    - a pullback has operator [&]
    - a parallel composition has operator [|]
    - a sum has operator [+]
    - a composition has operator [@]
    - a composition without hiding has operator [*]
    - the operator for an operation on strategies is the base operator 
    - the operator for an operation on games is the base operator followed by
    a [:] (if it makes sense to have one).
    *)
    
    (** {6 Pullback} *)
    
    (** Pullback of two strategies on the same game. *)
    val (&&&) : strategy -> strategy -> strategy
end

module Make (Pullback : Pullback.S) : S
