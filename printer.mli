
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

(** Pretty-printing of the defined datatypes *)

open Datatypes

(** {6 DOT format} *)

(** Outputs the given strategy, in DOT format, to the given formatter. *)
val dotOfStrategy : Format.formatter -> strategy -> unit

(** Outputs the given game, in DOT format, to the given formatter. *)
val dotOfGame : Format.formatter -> game -> unit

(** Outputs the given strategy, in DOT format, in the most verbose possible
    way. Only suitable for debug. *)
val dotDebugOfStrategy : Format.formatter -> strategy -> unit

(** {6 Direct display} *)

(** [dispDot displayer x] takes as its first argument [displayer], a function
    that outputs [x] in Dot format on a given [Format.formatter], and an object
    [x] to display, and opens a Dot window displaying it.
    This assumes that your system has a [dot] program and [feh] installed,
    and runs the command [dot -Tpng | feh -].
*)
val dispDot : (Format.formatter -> 'a -> unit) -> 'a -> unit

(** Composes {!dispDot} with {!dotOfStrategy}. *)
val dispStrategy : strategy -> unit

(** Composes {!dispDot} with {!dotOfGame}. *)
val dispGame : game -> unit

(** Composes {!dispDot} with {!dotDebugOfStrategy}. *)
val dispDebugStrategy : strategy -> unit

