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

(** Extensions of standard library modules. *)

module Array : sig
    include module type of Array
    val fold_left_i : (int -> 'a -> 'b -> 'a) -> 'a -> 'b array -> 'a
    val exists : ('a -> bool) -> 'a array -> bool
end

module List : sig
    include module type of List
    val map_option : ('a -> 'b option) -> 'a list -> 'b list
end

