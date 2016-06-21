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

module type S = sig
    exception NotComposed of game
    val extractGames : strategy -> strategy -> game list *game list *game list
    val compInteraction : strategy -> strategy -> strategy
    val compHidden : strategy -> strategy -> strategy
end 

module Canonical (Pullback : Pullback.S) (Parallel : Parallel.S) = struct
    exception NotComposed of game
    
    (** Extracts A, B, C three games from the parallel composed strategies
     such that B is maximal and st1 : S -> A || B, st2 : T -> B || C *)
    let extractGames st1 st2 =
        let isComposite game = Array.length game.g_parallel > 0 in
        if not @@ isComposite st1.st_game then
            raise @@ NotComposed st1.st_game
        else if not @@ isComposite st2.st_game then
            raise @@ NotComposed st2.st_game
        else
            (* N^2 algorithm, should be fast enough for what we want. *)
            let inCenter = ref GameSet.empty in
            let commonSet, leftSet = Array.fold_left_i
                (fun i (curCommon, curLeft) subGame ->
                    if Array.exists (fun x ->
                            Helpers.esp_eventsEquality x.g_esp subGame.g_esp)
                                st2.st_game.g_parallel then begin
                        inCenter := GameSet.add subGame !inCenter ;
                        (subGame :: curCommon, curLeft)
                    end else
                        (curCommon, subGame :: curLeft)
                ) ([],[]) st1.st_game.g_parallel in 
            let rightSet = Array.fold_left_i
                (fun i cur subGame ->
                    if not @@ GameSet.exists
                            (Helpers.gamesEqualityNoPol subGame) !inCenter
                        then subGame::cur
                        else cur) [] st2.st_game.g_parallel in
            leftSet, commonSet, rightSet

    let gameOfParallels parallels = match parallels with
    | [] -> Builder.game_empty
    | hd::[] -> hd
    | hd::tl -> (* Where tl <> [] *)
        List.fold_left Parallel.parallelGame hd tl
    
    (**
     Same as {!compInteraction}, but also returns the game on which we are
     working (cf {!extractGames}). This is useful for {!compHidden}.
    *)
    let annotatedInteraction st1 st2 =
        let leftGames, commonGames, rightGames = extractGames st1 st2 in
        let leftGame = gameOfParallels leftGames
        and rightGame= gameOfParallels rightGames in
        Pullback.pullback
            (Parallel.parallelStrat st1 (Builder.strat_id rightGame))
            (Parallel.parallelStrat (Builder.strat_id leftGame) st2),
                leftGames, commonGames, rightGames

    let compInteraction st1 st2 =
        (fun (x,_,_,_) -> x) @@ annotatedInteraction st1 st2
    
    let compHidden st1 st2 =
        let interact, leftGames, commonGames, rightGames =
            annotatedInteraction st1 st2 in
        let endGame = Parallel.parallelGame
            (gameOfParallels rightGames) 
            (gameOfParallels leftGames) in
        let events = NodeSet.filter (fun x ->
            NodeSet.mem (NodeMap.find x interact.st_map) endGame.g_esp.evts)
            interact.st_strat.evts in
        let map = NodeMap.filter (fun _ toEvt ->
            NodeSet.mem toEvt endGame.g_esp.evts) interact.st_map in
        let pol = NodeMap.filter (fun evt _ ->
            NodeSet.mem evt events) interact.st_strat.pol in
        {
            st_game = endGame ;
            st_map = map ;
            st_strat = {
                    evts = events;
                    pol = pol
                }
        }
end

