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

{
    open Lexing
    open LlamParser
    exception LexicalError of string

    let specIdents = [
        "P", Tprogtype ;
        "C", Tchantype ]

    let newline lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
            { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}
let varHead = ['a'-'z' 'A'-'Z']
let var = [ 'a'-'z' 'A'-'Z' '0'-'9' '_' ]

rule token = parse
| '\n'                          { newline lexbuf ; token lexbuf }
| [' ' '\t']+                   { token lexbuf }
| '('                           { Tlpar }
| ')'                           { Trpar }
| '['                           { Tlbra }
| ']'                           { Trbra }
| ','                           { Tcomma }
| ':'                           { Tcolon }
| ';'                           { Tsemicolon }
| "||"                          { Tparallel }
| '-'                           { Tdash }
| '~'                           { Ttilde }
| '*'                           { Ttensor }
| "->" | "→"                    { Tarrow }
| ['.']                         { Tdot }
| ['\\' '^'] | "λ"              { Tlambda }
| '&' | "ν"                     { Tnu }
| (varHead var*) as v           { try List.assoc v specIdents
                                  with Not_found -> Tvar(v) }
| '0'                           { Tzero }
| '1'                           { Tone }
| _ as c                        { raise @@ LexicalError ("Illegal character: \
                                        '"^(String.make 1 c)^"'.") }
| eof                           { Teof }
