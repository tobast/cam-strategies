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

(** Conversion function from linear lambda-calculus to games and strategies. *)

open LlamAst
open Datatypes

(** Raised if a variable has no assigned type. *)
exception UnboundVar of lamVar

(** Raised if a term cannot be typed. *)
exception BadTyping of lamTerm

(** [typeOf typEnv env term]
    retrieves the type of a term in an environment, and a type environment.
    If an atomic type is not found in the latter, it is assumed to be a
    "real" atomic type; else, the mapped type is inlined. *)
val typeOf : typeEnv -> typeEnv -> lamTerm -> lamType

(** [gameOfType typ] creates a game from a given type. *)
val gameOfType : lamType -> game

(** Composition of {!typeOf} and {!gameOfType}. *)
val gameOfTerm : typeEnv -> typeEnv -> lamTerm -> game

(** [stratOfTerm typEnv env term]
    creates a strategy from [term], using [env] as the type environment for
    variables, and [typEnv] as the type environment for types (see
    {!typeOf}). *)
val stratOfterm : typeEnv -> typeEnv -> lamTerm -> strategy

