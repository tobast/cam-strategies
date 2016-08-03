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

(** Pretty-printing of a lambda-term type. *)
val printType : Format.formatter -> LlamAst.lamType -> unit

(** Pretty-printing of a lambda-term. *)
val printLambda : Format.formatter -> LlamAst.lamTerm -> unit

(** Same as {!printLambda}, but with lots of braces. Debug purpose. *)
val printLambdaBraced : Format.formatter -> LlamAst.lamTerm -> unit
