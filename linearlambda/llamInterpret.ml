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
| LamAbs(_, vTyp, absTerm) ->
    LamArrow(expandType typEnv vTyp, typeOf typEnv env absTerm)
    
let rec gameOfType typ = Builder.(Operations.Canonical.(match typ with
| LamAtom(atom) ->
    snd @@ game_addNamedEvent atom PolNeg game_empty
| LamArrow(l,r) ->
    let lGame = perp @@ gameOfType l in
    let rGame = gameOfType r in
    lGame |||: rGame
))

let gameOfTerm typEnv env term =
    gameOfType @@ typeOf typEnv env term

let rec stratOfTerm typEnv env term = Builder.(match term with
| LamVar v -> copycat (gameOfType @@ findType env v)
| LamAbs(v,vTyp,absTerm) ->
        (strat_id @@ perp @@ gameOfType vTyp) |||
            (stratOfTerm typEnv (SMap.add v vTyp env) term)
| LamApp(u,v) -> assert false (*TODO*)
)

