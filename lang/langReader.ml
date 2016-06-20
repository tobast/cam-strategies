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
open Builder
open ParseTypes

type annotatedGame = game * dagNode SMap.t

type path = string

exception UndefinedEvent of idt

let extractNode map idt =
    (try SMap.find idt map
    with Not_found -> raise (UndefinedEvent idt))
    
let extractName id = function
| None -> id
| Some s -> s

let espOfDecls l =
    let rec doBuild ndMap cEsp = function
    | (ParseTypes.EspEvent evt)::tl ->
        let node,nEsp = esp_addNamedEvent (extractName evt.e_id evt.e_name)
                evt.e_pol cEsp in
        doBuild (SMap.add evt.e_id node ndMap) nEsp tl
    | (ParseTypes.EspEdge edge)::tl ->
        esp_addEdge
            (extractNode ndMap (fst edge)) (extractNode ndMap (snd edge)) ;
        doBuild ndMap cEsp tl
    | [] -> cEsp,ndMap
    in
    doBuild (SMap.empty) esp_empty l
    
let stratOfDecls (game, gameMap) decls =
    let referredNode id =
        extractNode gameMap id in
    
    let rec doBuild ndMap cStrat = function
    | (ParseTypes.StratEvent evt)::tl ->
        let node,nStrat = strat_addNamedEvent (extractName evt.s_id evt.s_name)
            (referredNode evt.s_map) cStrat in
        doBuild (SMap.add evt.s_id node ndMap) nStrat tl
    | (ParseTypes.StratEdge edge)::tl ->
        strat_addEdge
            (extractNode ndMap (fst edge)) (extractNode ndMap (snd edge)) ;
        doBuild ndMap cStrat tl
    | [] -> cStrat
    in
    
    doBuild SMap.empty (Builder.strat_new game) decls
    
(**************)
    
let extractGame = fst
    
let readAnnotatedGame lexbuf =
    let annGame = espOfDecls (StratlangParser.esp StratlangLexer.token lexbuf)
    in
    
    { g_esp = fst annGame ;
      g_parallel = [| |]
    }, snd annGame
    
let readGame lexbuf = extractGame @@ readAnnotatedGame lexbuf

let readStrategy annGame lexbuf =
    let decls = StratlangParser.strategy StratlangLexer.token lexbuf in
    stratOfDecls annGame decls
    
let lexbufOfFile path = Lexing.from_channel @@ Pervasives.open_in path

let readAnnotatedGameFile path =
    readAnnotatedGame @@ lexbufOfFile path

let readGameFile path =
    readGame @@ lexbufOfFile path
    
let readStrategyFile annGame path =
    readStrategy annGame @@ lexbufOfFile path

