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

open Datatypes

module type S = sig
    val (&&&) : strategy -> strategy -> strategy
    val (|||:) : game -> game -> game
    val (|||) : strategy -> strategy -> strategy
end

module Make
        (Pullback : Pullback.S)
        (Parallel : Parallel.S)
= struct
    let (&&&) = Pullback.pullback
    
    let (|||:) = Parallel.parallelGame
    let (|||) = Parallel.parallelStrat
end

module Canonical = Make(Pullback.Canonical)(Parallel.Canonical)

