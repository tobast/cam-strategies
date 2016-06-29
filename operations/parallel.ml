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
    val parallelGame : game -> game -> game
    val parallelGame_mapped : game -> game ->
        game * (dagNode NodeMap.t) * (dagNode NodeMap.t)
    val parallelEsp : esp -> esp -> esp
    val parallelEsp_mapped : esp -> esp ->
        esp * dagNode NodeMap.t * dagNode NodeMap.t
    val parallelStrat : strategy -> strategy -> strategy
    val parallelStrat_mapped : strategy -> strategy ->
        strategy * dagNode NodeMap.t * dagNode NodeMap.t
end

module Canonical = struct
    let mappedNode map nd =
        (try NodeMap.find nd map
        with Not_found -> nd)

    let parallelGame_mapped g1 g2 =
        Builder.game_parallel_mapped g1 g2
        
    let parallelGame g1 g2 =
        (fun (x,_,_) -> x) @@ parallelGame_mapped g1 g2
        
    let parallelEsp_mapped esp1 esp2 =
        let ce1,m1 = Builder.esp_copy_mapped esp1 in
        let ce2,m2 = Builder.esp_copy_mapped esp2 in
        let pol = NodeMap.merge Helpers.mapMerger ce1.pol ce2.pol in
        {
            evts = NodeSet.union ce1.evts ce2.evts ;
            pol = pol
        },m1,m2
    
    let parallelEsp esp1 esp2 = (fun (x,_,_) -> x) @@
        parallelEsp_mapped esp1 esp2
    
    let parallelStrat_mapped s1 s2 =
        let game,gameMap1,gameMap2 = parallelGame_mapped
            s1.st_game s2.st_game in
        let strat,stratMap1,stratMap2 = parallelEsp_mapped
            s1.st_strat s2.st_strat in
        
        let remapMap sMap gMap map = NodeMap.fold (fun key nd cur ->
                NodeMap.add (mappedNode sMap key)
                (mappedNode gMap nd) cur) map
        in
        let map = remapMap stratMap1 gameMap1 s1.st_map (
            remapMap stratMap2 gameMap2 s2.st_map NodeMap.empty) in
        
        {
            st_game = game;
            st_strat = strat;
            st_map = map
        }, stratMap1, stratMap2
        
    let parallelStrat s1 s2 = (fun (x,_,_) -> x) @@ parallelStrat_mapped s1 s2
end

