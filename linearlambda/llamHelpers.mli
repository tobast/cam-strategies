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

(** Functions related to linear lambda-calculus that don't fit a particular
    module. *)

(** Interprets a string as a lambda-term. *)
val lambdaize : string -> LlamAst.lamTerm

(** Renames variables (adding [']) so that no two distinct variables have the
    same name. *)
val disambiguate : LlamAst.lamTerm -> LlamAst.lamTerm

