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
open ModExtensions

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

exception MergeConflict
let mapMerger _ x y = match (x,y) with
| Some a, Some b -> if a = b then Some a else raise MergeConflict
| None, Some x | Some x, None -> Some x
| None,None -> None

let esp_eventsEquality e1 e2 = e1.evts = e2.evts

let rec gamesEqualityNoPol g1 g2 =
    if Array.length g1.g_parallel <> Array.length g2.g_parallel then
        false
    else if Array.length g1.g_parallel = 0 then
        esp_eventsEquality g1.g_esp g2.g_esp
    else
        Array.fold_left_i (fun i cur gm ->
                cur && (gamesEqualityNoPol gm g2.g_parallel.(i)))
            true g1.g_parallel
            
let gameIn g gSuper =
    if Array.length gSuper.g_parallel = 0 then
        gamesEqualityNoPol g gSuper
    else Array.fold_left (fun cur cGame ->
        cur || gamesEqualityNoPol g cGame) false gSuper.g_parallel

let gameIncluded g1 g2 =
    if Array.length g1.g_parallel = 0 then
        gameIn g1 g2
    else
        Array.fold_left (fun cur subgame ->
            cur && gameIn subgame g2) true g1.g_parallel
            
let selfNodeMap elts =
    NodeSet.fold (fun elt cur -> NodeMap.add elt elt cur) elts NodeMap.empty

let eventsEqual e1 e2 =
    getBaseId e1.nodeId = getBaseId e2.nodeId

