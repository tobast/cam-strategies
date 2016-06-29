open Datatypes
open LangReader
open Builder
open Operations.Canonical

(*let basePath = "strategies/testAssoc/"
let annGame = readAnnotatedGameFile (basePath^"baseStrat.game")
let game = extractGame annGame *)
(*let strat = readStrategyFile annGame (basePath^"baseStrat.strat") in*)

let mkNamedStrat name =
    let posGameEvt, nGame =
        game_addNamedEvent (name^"pos") PolPos game_empty in
    let negGameEvt, nGame =
        game_addNamedEvent (name^"neg") PolNeg nGame in
    game_addEdge posGameEvt negGameEvt;
    
    strat_newFilled nGame

let stratA = mkNamedStrat "A_" 
let stratB = mkNamedStrat "B_" 
let stratC = mkNamedStrat "C_" 
let stratD = mkNamedStrat "D_" 
let stratE = mkNamedStrat "E_" 
let stratF = mkNamedStrat "F_"

let strat = (stratA ||| stratB) ||| ((stratC ||| stratD) ||| stratE ||| stratF)

let dumpGameTree game =
    let tree = (match game.g_tree with
        | None -> TreeLeaf(game)
        | Some x -> x) in
    Helpers.dumpTreeStructure Format.std_formatter tree;
    Format.printf "###########################@." 
let dumpStratTree strat = dumpGameTree strat.st_game

let dispGame = (*Printer.dispGame*) ignore
    
let () =
    let game = strat.st_game in
    dispGame game ;
    dumpGameTree game ;
    NodeSet.iter (fun nd -> let way = (match nd.nodeId with CompId(x,_) -> x) in
        Format.printf "%s %a@." nd.nodeName Helpers.dispWay way) game.g_esp.evts ;
    let nGame = game_reassoc game
        (TreeNode(TreeLeaf "X", TreeLeaf "Y"))
        (TreeNode(TreeLeaf "Y", TreeLeaf "X")) in
    dumpGameTree nGame ;
    NodeSet.iter (fun nd -> let way = (match nd.nodeId with CompId(x,_) -> x) in
        Format.printf "%s %a@." nd.nodeName Helpers.dispWay way) nGame.g_esp.evts ;
    dispGame nGame

