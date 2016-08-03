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

%{
    open LlamAst
    exception ParseError of string

    let makeAbstraction decls term =
        List.fold_right (fun (var,typ) cur -> LamAbs(var,typ, cur))
            decls term

    let oppositeCh = function
    | CcsCh(x) -> CcsOppCh(x)
    | CcsOppCh(x) -> CcsCh(x)

    let chkChan isChan typ name =
        let strOfCase = match isChan with
        | true -> "must be a channel here."
        | false -> "cannot be a channel here."
        in
        if (isChan && typ <> CcsChan) || (not isChan && typ = CcsChan) then
            raise @@ ParseError (name ^ " " ^ strOfCase)

    let chName = function
    | CcsCh(x) | CcsOppCh(x) -> x
%}

%token Tprogtype Tchantype
%token Tlpar Trpar Tcomma Tcolon Tsemicolon Tparallel Ttensor Tdot Tarrow
%token Tlbra Trbra Tlambda Tnu Tone Tzero Tdash Ttilde Teof
%token <string> Tvar

%start <LlamAst.lamTerm> term

%right Tarrow
%nonassoc Tnu
%right Tsemicolon Tparallel
%nonassoc Tdot Tlambda Tdash
%nonassoc Tvar Tzero Tone Tlbra
%nonassoc Tlpar
%right Ttensor
%right appl
%%

term:
    l=lambdaTerm ; Teof                             { l }

lambdaTerm:
| v = Tvar                                          { LamVar(StrVar(v)) }
| cv = ccsChanVar                                   { LamVar(ChVar(cv)) }
| Tlambda ;
    decls = separated_nonempty_list(Tcomma, absDecl) ;
    Tdot ;
    t = lambdaTerm                       %prec Tdot { makeAbstraction decls t }
| Tlpar ; l = lambdaTerm ; Trpar                    { l }
| l = lambdaTerm ; r = lambdaTerm        %prec appl { LamApp(l,r) }
| l = lambdaTerm ; Ttensor ; r = lambdaTerm         { LamTensor(l,r) }
| ch = ccsChanVar ; Tdash ; t = lambdaTerm          { CcsCallChan(ch, t)}
| Tone                                              { CcsOne }
| Tzero                                             { CcsZero }
| l = lambdaTerm ; Tparallel ; r = lambdaTerm       { CcsParallel(l,r) }
| l = lambdaTerm ; Tsemicolon; r = lambdaTerm       { CcsSeq(l,r) }
| Tlpar; Tnu; v=ccsChanVar; Trpar ; t = lambdaTerm
                                       %prec Tnu    { CcsNew(v,t) }

absDecl:
| v=Tvar ; Tcolon ; typ=lambdaType                  { chkChan false typ v ;
                                                        (StrVar v,typ) }
| v = ccsChanVar ; Tcolon ; typ=lambdaType          { chkChan true typ
                                                        (chName v) ;
                                                        (ChVar v,typ) }

lambdaType:
| l=lambdaType ; Tarrow ; r=lambdaType    %prec Tarrow
                                                    { LamArrow(l,r) }
| Tlpar ; typ = lambdaType ; Trpar                  { typ }
| Tprogtype                                         { CcsProg }
| Tchantype                                         { CcsChan }
| l=lambdaType; Ttensor ; r=lambdaType              { LamTensorType(l,r) }

ccsChanVar:
| Tlbra; ch = ccsInnerChanVar ; Trbra               { ch }

ccsInnerChanVar:
| Ttilde; ch = ccsInnerChanVar                      { oppositeCh ch }
| ch = Tvar                                         { CcsCh(ch) }
