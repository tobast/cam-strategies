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
exception UnboundVar of lccsVar

(** Raised if a term cannot be typed. *)
exception BadTyping of lamTerm

(** Raised if a term is not linear *)
exception NonLinearTerm

exception NotImplemented

(** [typeOfTerm term]
    retrieves the type of a term. *)
val typeOfTerm : lamTerm -> lamType

(** [stratOfTerm term]
    creates a strategy from [term]. *)
val stratOfTerm : lamTerm -> strategy

