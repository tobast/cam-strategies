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
| CcsProg -> Format.fprintf fmt "ℙ"
| CcsChan -> Format.fprintf fmt "ℂ"
| LamTensorType(lt,rt) -> Format.fprintf fmt "%a * %a"
                                            printType lt printType rt
| LamArrow(l,r) ->
    (match l with
    | CcsProg | CcsChan -> Format.fprintf fmt "%a → %a" printType l printType r
    | LamTensorType _
    | LamArrow _ -> Format.fprintf fmt "(%a) → %a" printType l printType r)

let printChan fmt = function
| CcsCh(x) -> Format.fprintf fmt "[%s]" x
| CcsOppCh(x) -> Format.fprintf fmt "[¬%s]" x

let printVar fmt = function
| StrVar(x) -> Format.fprintf fmt "%s" x
| ChVar(ch) -> Format.fprintf fmt "%a" printChan ch

let rec printLambda fmt = function
| LamVar(x) -> Format.fprintf fmt "%a" printVar x
| LamApp(m,n) -> Format.fprintf fmt "%a %a" printLambda m printLambda n
| LamAbs(v,typ,t) ->
        Format.fprintf fmt "(λ%a : %a · %a)"
            printVar v printType typ printLambda t
| LamTensor(l,r) ->
        Format.fprintf fmt "(%a * %a)" printLambda l printLambda r
| CcsZero -> Format.fprintf fmt "0"
| CcsOne -> Format.fprintf fmt "1"
| CcsCallChan(ch,x) -> Format.fprintf fmt "%a - (%a)" printChan ch printLambda x
| CcsParallel(l,r) -> Format.fprintf fmt "(%a) || (%a)" printLambda l printLambda r
| CcsSeq(l,r) -> Format.fprintf fmt "(%a) ; (%a)" printLambda l printLambda r
| CcsNew(v,t) -> Format.fprintf fmt "(ν%a) (%a)" printChan v printLambda t

let rec printLambdaBraced fmt = function
| LamVar(x) -> Format.fprintf fmt "(%a)" printVar x
| LamApp(m,n) ->
        Format.fprintf fmt "(%a %a)" printLambdaBraced m printLambdaBraced n
| LamAbs(v,typ,t) ->
        Format.fprintf fmt "(λ%a : %a · (%a))"
            printVar v printType typ printLambdaBraced t
| LamTensor(l,r) ->
        Format.fprintf fmt "(%a * %a)" printLambdaBraced l printLambdaBraced r
| CcsZero -> Format.fprintf fmt "0"
| CcsOne -> Format.fprintf fmt "1"
| CcsCallChan(ch,x) ->
        Format.fprintf fmt "(%a - %a)" printChan ch printLambdaBraced x
| CcsParallel(l,r) ->
        Format.fprintf fmt "(%a || %a)" printLambdaBraced l printLambdaBraced r
| CcsSeq(l,r) ->
        Format.fprintf fmt "(%a ; %a)" printLambdaBraced l printLambdaBraced r
| CcsNew(v,t) ->
        Format.fprintf fmt "((ν%a) %a)" printChan v printLambdaBraced t

