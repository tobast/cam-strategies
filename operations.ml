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
    val perp : game -> game
    val (&&&) : strategy -> strategy -> strategy
    val (|||:) : game -> game -> game
    val (|||) : strategy -> strategy -> strategy
    val ( *** ) : strategy -> strategy -> strategy
    val interaction : strategy -> strategy -> game -> strategy
    val (@@@) : strategy -> strategy -> strategy
    val composition : strategy -> strategy -> game -> strategy
end

module Make
        (Pullback : Pullback.S)
        (Parallel : Parallel.S)
        (Compose : Composition.S)
= struct
    (** Warning! This implementation only modifies pol without copying
        anything. *)
    let perp game =
        let oppPol = function
        | PolPos -> PolNeg
        | PolNeg -> PolPos
        | PolNeutral -> PolNeutral in
        let nEsp = { game.g_esp
            with pol = NodeMap.map oppPol game.g_esp.pol } in
        { game with g_esp = nEsp }
        
    let (&&&) = Pullback.pullback
    
    let (|||:) = Parallel.parallelGame
    let (|||) = Parallel.parallelStrat
    
    let ( *** ) = Compose.compInteraction
    let interaction = Compose.compInteractionOnGame
    let ( @@@ ) = Compose.compHidden
    let composition = Compose.compHiddenOnGame
end


module Canonical = Make
    (Pullback.Canonical)
    (Parallel.Canonical)
    (Composition.Canonical (Pullback.Canonical) (Parallel.Canonical))

