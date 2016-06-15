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

let getPolarity nd esp =
    (try NodeMap.find nd esp.pol
    with Not_found -> raise InvalidStrategy)
    
let getGameNode nd strat =
    (try NodeMap.find nd strat.st_map
    with Not_found -> raise InvalidStrategy)
    
let nextId =
    let cId = ref (-1) in (* -1: will be incremented on first use. *)
    (fun () -> incr cId; !cId)
    
let map2 f1 f2 (x,y) =
    f1 x, f2 y
let id x = x

