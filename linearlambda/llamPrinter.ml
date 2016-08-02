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

let rec printCcsType fmt = function
| CcsProg -> Format.fprintf fmt "ℙ"
| CcsChan -> Format.fprintf fmt "ℂ"

let rec printType fmt = function
| LamCcsType(typ) -> Format.fprintf fmt "%a" printCcsType typ
| LamTensorType(lt,rt) -> Format.fprintf fmt "%a * %a"
                                            printType lt printType rt
| LamArrow(l,r) ->
    (match l with
    | LamCcsType _ -> Format.fprintf fmt "%a → %a" printType l printType r
    | LamTensorType _
    | LamArrow _ -> Format.fprintf fmt "(%a) → %a" printType l printType r)

let rec printCcs fmt = function
| CcsZero -> Format.fprintf fmt "0"
| CcsOne -> Format.fprintf fmt "1"
| CcsOppVar(x) -> Format.fprintf fmt "¬%s" x
| CcsVar(x) -> Format.fprintf fmt "%s" x
| CcsParallel(l,r) -> Format.fprintf fmt "%a || %a" printCcs l printCcs r
| CcsSeq(l,r) -> Format.fprintf fmt "%a ; %a" printCcs l printCcs r
| CcsNew(v,t) -> Format.fprintf fmt "(ν%s) %a" v printCcs t


let rec printLambda fmt = function
| LamVar(x) -> Format.fprintf fmt "%s" x
| LamApp(m,n) -> Format.fprintf fmt "%a %a" printLambda m printLambda n
| LamAbs(v,typ,t) ->
        Format.fprintf fmt "(λ%s : %a · %a)"
            v printType typ printLambda t
| LamTensor(l,r) ->
        Format.fprintf fmt "(%a * %a)" printLambda l printLambda r
| LamCcs(ccs) ->
        Format.fprintf fmt "%a" printCcs ccs

