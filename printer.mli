
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

