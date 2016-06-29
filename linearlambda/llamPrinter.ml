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

let rec printType fmt = function
| LamAtom(id) -> Format.fprintf fmt "%s" id
| LamArrow(l,r) ->
    (match l with
    | LamAtom _ -> Format.fprintf fmt "%a → %a" printType l printType r
    | LamArrow _ -> Format.fprintf fmt "(%a) → %a" printType l printType r)

let rec printLambda fmt = function
| LamVar(x) -> Format.fprintf fmt "%s" x
| LamApp(m,n) -> Format.fprintf fmt "%a %a" printLambda m printLambda n
| LamAbs(v,typ,t) ->
        Format.fprintf fmt "(λ%s : %a · %a)"
            v printType typ printLambda t

