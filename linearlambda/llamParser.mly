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

    let makeAbstraction decls term =
        List.fold_right (fun (var,typ) cur -> LamAbs(var,typ, cur))
            decls term
%}

%token Tprogtype Tchantype
%token Tlpar Trpar Tcomma Tcolon Tsemicolon Tparallel Ttensor Tdot Tarrow
%token Tlambda Tnu Tone Tzero Tdash Ttilde Teof
%token <string> Tvar

%start <LlamAst.lamTerm> term

%right Tarrow
%nonassoc Tdot Tlambda Tnu Ttilde Tdash
%nonassoc Tvar Tzero Tone
%nonassoc Tlpar
%right Ttensor
%right Tsemicolon Tparallel
%right appl
%%

term:
    l=lambdaTerm ; Teof                             { l }

lambdaTerm:
| v = Tvar                                          { LamVar(v) }
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
| Tlpar; Tnu; v=Tvar; Trpar ; t = lambdaTerm
                                       %prec Tnu    { CcsNew(v,t) }

absDecl:
| v=Tvar ; Tcolon ; typ=lambdaType                  { (v,typ) }

lambdaType:
| l=lambdaType ; Tarrow ; r=lambdaType    %prec Tarrow
                                                    { LamArrow(l,r) }
| Tlpar ; typ = lambdaType ; Trpar                  { typ }
| Tprogtype                                         { CcsProg }
| Tchantype                                         { CcsChan }
| l=lambdaType; Ttensor ; r=lambdaType              { LamTensorType(l,r) }

ccsChanVar:
| Ttilde; ch = Tvar                                 { CcsOppCh(ch) }
| ch = Tvar                                         { CcsCh(ch) }
