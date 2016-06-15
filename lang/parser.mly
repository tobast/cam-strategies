%{
open Datatypes
open Builder

type idt = string
type tEspEvent = { e_id : idt ; e_name : string option ; e_pol : polarity }
type tStratEvent = { s_id : idt ; s_name : string option ; s_map : idt }

type tEdge = idt*idt

type tEspDecl = EspEdge of tEdge | EspEvent of tEspEvent
type tStratDecl = StratEdge of tEdge | StratEvent of tStratEvent

exception UndefinedEvent of idt

let extractNode map idt =
    (try SMap.find idt map
    with Not_found -> raise (UndefinedEvent idt))
    
let extractName = function
| None -> ""
| Some s -> s

let espOfDecls l =
    let rec doBuild ndMap cEsp = function
    | (EspEvent evt)::tl ->
        let node,nEsp = esp_addNamedEvent (extractName evt.e_name)
                evt.e_pol cEsp in
        doBuild (SMap.add evt.e_id node ndMap) nEsp tl
    | (EspEdge edge)::tl ->
        esp_addEdge
            (extractNode ndMap (fst edge)) (extractNode ndMap (snd edge)) ;
        doBuild ndMap cEsp tl
    | [] -> cEsp,ndMap
    in
    doBuild (SMap.empty) esp_empty l

%}

%token Tlpar Trpar Tlbracket Trbracket Tarrow Tplus Tminus Teof
%token <string> Tident

%start <tStratDecl list> int_strategy

%start <Datatypes.esp * dagNode SMap.t> esp

%%

esp:
   decl = espDecl* ; Teof       { espOfDecls decl }

int_strategy:
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


