open LangReader
open Datatypes
open Operations.Canonical

let basepath = "strategies/compose_aba/"
let gmA = readGameFile (basepath ^ "A.game")
let gmB = readGameFile (basepath ^ "B.game")
let perpA = perp gmA
and perpB = perp gmB

let gmA2 = Builder.game_copy gmA

let nodeByName map name = SMap.find name map

let gmPA_B = perpA |||: gmB
let stratPA_B, namesPA_B = Builder.strat_newFilled_mapped gmPA_B
let () =
    Builder.strat_addEdge (nodeByName namesPA_B "Apos2")
        (nodeByName namesPA_B "Aneg") ;
    Builder.strat_addEdge (nodeByName namesPA_B "Apos1")
        (nodeByName namesPA_B "Bpos") ;
    Builder.strat_addEdge (nodeByName namesPA_B "Bneg1")
        (nodeByName namesPA_B "Bpos") ;
    Builder.strat_addEdge (nodeByName namesPA_B "Bneg2")
        (nodeByName namesPA_B "Bpos") ;
    Builder.strat_addEdge (nodeByName namesPA_B "Bneg2")
        (nodeByName namesPA_B "Aneg")
        
let gmPB_A = perpB |||: gmA
let stratPB_A, namesPB_A = Builder.strat_newFilled_mapped gmPB_A
let () =
    Builder.strat_addEdge (nodeByName namesPB_A "Bpos")
        (nodeByName namesPB_A "Apos1") ;
    Builder.strat_addEdge (nodeByName namesPB_A "Bpos")
        (nodeByName namesPB_A "Apos2") ;
    Builder.strat_addEdge (nodeByName namesPB_A "Aneg")
        (nodeByName namesPB_A "Apos1") ;
    Builder.strat_addEdge (nodeByName namesPB_A "Aneg")
        (nodeByName namesPB_A "Apos2") ;
    Builder.strat_addEdge (nodeByName namesPB_A "Aneg")
        (nodeByName namesPB_A "Bneg2")

(*let () = Printer.dotDebugOfStrategy Format.std_formatter stratCompo*)
let () =
(*    Printer.dispDebugStrategy (stratPA_B *** stratPB_A) ; *)
    Printer.dispDebugStrategy (stratPA_B @@@ stratPB_A)
