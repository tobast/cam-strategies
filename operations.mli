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
    - the mapped operator, when available, is postfixed with a [~].
    *)

    (** {6 Unary} *)

    (** Dual game *)
    val perp : game -> game

    (** Copycat strategy of a game. *)
    val copycat : game -> strategy

    type copycatSide = CcLeft | CcRight

    (** Copycat strategy, where strategy events bear the names given by
        a function, based on its node in the new game and its side on
        the copycat. {b Warning}: the function receives the node in the
        {e new} game, which is {e not} the node in the given game.
    *)
    val copycat_named : (dagNode -> copycatSide -> string) -> game -> strategy

    (** {6 Pullback} *)

    (** Pullback of two strategies on the same game. *)
    val (&&&) : strategy -> strategy -> strategy

    (** {6 Parallel composition} *)

    (** Parallel composition of two games *)
    val (|||:) : game -> game -> game

    (** Same as {!(|||:)}, returning maps from both games to the new game. *)
    val (|||:~) : game -> game -> game * dagNode NodeMap.t * dagNode NodeMap.t

    (** Parallel composition of two strategies *)
    val (|||) : strategy -> strategy -> strategy

    (** Same as {!(|||)}, returning maps from both strategies to the new
        strategy. *)
    val (|||~) : strategy -> strategy ->
        strategy * dagNode NodeMap.t * dagNode NodeMap.t

    (** {6 Composition} *)

    (** Composition interaction (ie. without hiding) of two strategies. *)
    val ( *** ) : strategy -> strategy -> strategy

    (** Composition (with hiding) of two strategies. *)
    val (@@@) : strategy -> strategy -> strategy
end

module Make
    (Pullback : Pullback.S)
    (Parallel : Parallel.S)
    (Compose : Composition.S)
    : S

module Canonical : S

