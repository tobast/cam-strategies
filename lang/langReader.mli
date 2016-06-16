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

(** Wrapper around lexer/parser to read strategies and games out of
 a custom format *)

open Datatypes

(** A game annotated having its events annotated with unique IDs *)
type annotatedGame

(** A file path (used for function types' clarity). *)
type path = string

(** Reads an annotated game from the given lexbuf *)
val readAnnotatedGame : Lexing.lexbuf -> annotatedGame

(** Same as {!readAnnotatedGame}, from a file. *)
val readAnnotatedGameFile : path -> annotatedGame

(** Same as {!extractGame} [@@] {!readAnnotatedGame} *)
val readGame : Lexing.lexbuf -> Datatypes.esp

(** Same as {!readGame}, from a file. *)
val readGameFile : path -> Datatypes.esp

(** Reads a strategy from the given lexbuf on the given annotated game.
 The annotations must match. *)
val readStrategy : annotatedGame -> Lexing.lexbuf -> strategy

(** Same as {!readStrategy}, from a file. *)
val readStrategyFile : annotatedGame -> path -> strategy

(** Extracts a {!Datatypes.game} from the given annotated game. *)
val extractGame : annotatedGame -> game

