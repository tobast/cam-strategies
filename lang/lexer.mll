{
    open Lexing
    open Parser
    
    exception LexicalError of string
    
    let lexicalError msg =
        raise (LexicalError msg)
    
    let newline lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
            { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

let whitespace = [ ' ' '\t' ]
let alphanum = [ 'a'-'z' 'A'-'Z' '0'-'9' '_' ]

rule token = parse
| '\n'                  	{ newline lexbuf ; token lexbuf }
| whitespace+           	{ token lexbuf }
| '('               		{ Tlpar }
| ')'               		{ Trpar }
| '['               		{ Tlbracket }
| ']'               		{ Trbracket }
| "->"              		{ Tarrow }
| '+'                       { Tplus }
| '-'                       { Tminus }
| "//" [^'\n']*             { token lexbuf } (* One-line comment *)
| "/*"                      { multilineComment lexbuf }
| alphanum* as ident        { Tident(ident) }
| eof                       { Teof }
| _ as c                    { lexicalError ("Illegal character: '"^c^"'") }

and multilineComment = parse
| "*/"                      { token lexbuf }
| [^'\n']                   { multilineComment lexbuf }
| '\n'                      { newline lexbuf ; multilineComment lexbuf }
| "/*"                      { lexicalError "Illegal nested comment." }
| eof                   { lexicalError "Reached EOF before end of comment." }
