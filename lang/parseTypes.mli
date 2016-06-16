open Datatypes

type idt = string

type tEspEvent = { e_id : idt ; e_name : string option ; e_pol : polarity }
type tStratEvent = { s_id : idt ; s_name : string option ; s_map : idt }

type tEdge = idt*idt

type tEspDecl = EspEdge of tEdge | EspEvent of tEspEvent
type tStratDecl = StratEdge of tEdge | StratEvent of tStratEvent

