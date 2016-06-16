%{
open Datatypes
open ParseTypes
%}

%token Tlpar Trpar Tlbracket Trbracket Tarrow Tplus Tminus Teof
%token <string> Tident

%start <ParseTypes.tStratDecl list> strategy

%start <ParseTypes.tEspDecl list> esp

%%

esp:
   decl = espDecl* ; Teof       { decl }

strategy:
    decl = stratDecl* ; Teof    { decl }

espDecl:
| e=edge                        { EspEdge(e) }
| ev=espEvent                   { EspEvent(ev) }

stratDecl:
| e=edge                        { StratEdge(e) }
| ev=stratEvent                 { StratEvent(ev) }

edge:
    fromN=Tident ; Tarrow; toN=Tident       { (fromN,toN) }

espEvent:
    id=Tident; n=name; p=polarity
                                { { e_id = id ; e_name = n ; e_pol = p } }

stratEvent:
    id=Tident; m=map; n=name    { { s_id = id; s_name = n; s_map = m } }

name:
    n=option(delimited(Tlbracket, Tident, Trbracket))
                                { n }

polarity:
|   Tplus                       { PolPos }
|   Tminus                      { PolNeg }

map:
    Tlpar; m=Tident; Trpar      { m }


