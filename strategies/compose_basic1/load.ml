open LangReader
open Datatypes
open Operations.Canonical

let basepath = "strategies/compose_basic1/"
let gmA = readGameFile (basepath^"A.game")
let annB = readAnnotatedGameFile (basepath^"B.game")
let annC = readAnnotatedGameFile (basepath^"C.game")
let perpA = perp gmA

let stratPA = Builder.strat_newFilled perpA
(*    let stratEsp = Builder.esp_copy perpA.g_esp in 
    {
        st_strat = stratEsp ;
        st_game = perpA ;
        st_map = NodeMap.singleton (NodeSet.choose stratEsp.evts)
            (NodeSet.choose gmA.g_esp.evts)
    } *)
    
let stratB = readStrategyFile annB (basepath^"B.strat")
let stratC = readStrategyFile annC (basepath^"C.strat")

let stratAB = stratPA ||| stratB
let () =
    let ndFrom = NodeSet.choose @@ NodeSet.filter
        (fun nd -> nd.nodeName = "Apos") stratAB.st_strat.evts in
    let ndTo = NodeSet.choose @@ NodeSet.filter
        (fun nd -> nd.nodeName = "Bpos") stratAB.st_strat.evts in
    Builder.strat_addEdge ndFrom ndTo

let perpB = perp (extractGame annB)
let stratPB = Builder.strat_newFilled perpB
let stratBC = stratPB ||| stratC
let () =
    let ndFrom = NodeSet.choose @@ NodeSet.filter
        (fun nd -> nd.nodeName = "Bpos") stratBC.st_strat.evts in
    let ndTo = NodeSet.choose @@ NodeSet.filter
        (fun nd -> nd.nodeName = "Cpos") stratBC.st_strat.evts in
    Builder.strat_addEdge ndFrom ndTo

module Comp = Composition.Canonical (Pullback.Canonical) (Parallel.Canonical)
let left::mid::right::[] = List.map List.hd ((fun (x,y,z) -> [x;y;z]) @@
    Comp.extractGames stratAB stratBC)
let leftId = Builder.strat_id left
and rightId= Builder.strat_id right

let parAB = stratAB ||| rightId and parBC = leftId ||| stratBC

let eq = Helpers.gamesEqualityNoPol

(*let () = Printer.dotDebugOfStrategy Format.std_formatter (stratAB *** stratBC) *)
let stratCompo = stratAB @@@ stratBC
    
let () = Printer.dotDebugOfStrategy Format.std_formatter (stratCompo)
