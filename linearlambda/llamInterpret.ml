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

open LlamAst
open Datatypes
open Operations.Canonical

exception UnboundVar of lamVar
exception BadTyping of lamTerm
exception NonLinearTerm

let findType env v =
    (try SMap.find v env
    with Not_found -> raise @@ UnboundVar v)
    
(** YES, this does not necessarily terminate. If there is a loop in the types'
    declaration, it's the user's problem. *)
let rec expandType env typ = match typ with
| LamAtom(t) ->
    (try expandType env (SMap.find t env)
    with Not_found -> typ)
| LamArrow(l,r) -> LamArrow(expandType env l, expandType env r)

let rec typeOf typEnv env term = match term with
| LamVar v -> expandType typEnv @@ findType env v
| LamApp(s,t) ->
    let lType = typeOf typEnv env s in
    let rType = typeOf typEnv env t in
    (match lType with
    | LamArrow(fromTyp,toTyp) ->
        if fromTyp = rType then
            toTyp
        else
            raise @@ BadTyping(term)
    | _ -> raise @@ BadTyping(term))
| LamAbs(v, vTyp, absTerm) ->
    LamArrow(expandType typEnv vTyp, typeOf typEnv
            (SMap.add v vTyp env) absTerm)
    
let gameOfType =
    let gameEnv = ref SMap.empty in
    let rec mkGame typ = Builder.(Operations.Canonical.(match typ with
    | LamAtom(atom) ->
        if not @@ SMap.mem atom !gameEnv then
            gameEnv := SMap.add atom
                (snd @@ game_addNamedEvent atom PolNeg game_empty) !gameEnv ;
        SMap.find atom !gameEnv
    | LamArrow(l,r) ->
        let lGame = perp @@ mkGame l in
        let rGame = mkGame r in
        lGame |||: rGame
    )) in
    mkGame

let gameOfTerm typEnv env term =
    gameOfType @@ typeOf typEnv env term

let splitEnv env lTerm rTerm =
    (* Returns (outEnv, digEnv \ {extracted}, alreadyExtracted u {extracted})*)
    let rec digEnv curEnv toSplit alreadyExtracted unwatched = function
    | LamVar(v) ->
        if SMap.mem v toSplit then (* Extract from environment *)
            SMap.add v (SMap.find v toSplit) curEnv,
                SMap.remove v toSplit,
                SSet.add v alreadyExtracted
        else if SSet.mem v alreadyExtracted then
            raise NonLinearTerm
        else if not @@ SSet.mem v unwatched then
            raise @@ UnboundVar v
        else
            curEnv, toSplit, alreadyExtracted
    | LamAbs(v,_,absTerm) ->
        digEnv curEnv toSplit alreadyExtracted (SSet.add v unwatched) absTerm
    | LamApp(lTerm,rTerm) ->
        let nEnv, nToSplit, nExtracted =
            digEnv curEnv toSplit alreadyExtracted unwatched lTerm in
        digEnv nEnv nToSplit nExtracted unwatched rTerm
    in
    
    let lEnv, nEnv, extracted =
        digEnv SMap.empty env SSet.empty SSet.empty lTerm in
    let rEnv, finalEnv, _ =
        digEnv SMap.empty nEnv extracted SSet.empty rTerm in
    if not @@ SMap.is_empty finalEnv then
        raise NonLinearTerm ;
    lEnv,rEnv

let rec stratOfTerm_env typEnv env term = Builder.(match term with
| LamVar v -> copycat (gameOfType @@ findType env v)
| LamAbs(v,vTyp,absTerm) ->
        strat_assocRight (
            (copycat @@ perp @@ gameOfType vTyp) |||
            (stratOfTerm_env typEnv (SMap.add v vTyp env) absTerm))
| LamApp(lTerm,rTerm) ->
    let lEnv,rEnv = splitEnv env lTerm rTerm in
    let ltermGame = (gameOfType @@ typeOf typEnv lEnv lTerm) in
    let ccStrat = copycat ((perp ltermGame) |||: ltermGame) in
    let lStrat = stratOfTerm_env typEnv lEnv lTerm
    and rStrat = stratOfTerm_env typEnv rEnv rTerm in
(*    Printer.dispDebugStrategy ccStrat ; *)
(*    Printer.dispDebugStrategy (lStrat ||| rStrat) ; *)
    Format.printf "%a@." LlamPrinter.printLambda term;
    Format.printf "[----------------------------@." ;
    Helpers.dumpTreeStructure Format.std_formatter (match ccStrat.st_game.g_tree with Some x -> x);
    Format.printf "-----------------------------@." ;
    Helpers.dumpTreeStructure Format.std_formatter (match (lStrat ||| rStrat).st_game.g_tree with Some x -> x);
    Format.printf "----------------------------]@." ;
    ccStrat @@@ (lStrat ||| rStrat)
)

let stratOfTerm = stratOfTerm_env SMap.empty SMap.empty

