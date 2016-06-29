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

open Datatypes
open ModExtensions

let newNode name = {
        nodeId = CompId(CompBase,Helpers.nextId ()); 
        nodeName = name ;
        nodeInEdges = [] ;
        nodeOutEdges = []
    }

let mappedNodeNF onNotFound ndMap nd =
    (try Some (NodeMap.find nd ndMap)
    with Not_found -> onNotFound nd)

let mappedNode map nd =
    (try NodeMap.find nd map
    with Not_found -> nd)

let mapEdgeNodes onNotFound ndMap edge =
    match (
            mappedNodeNF onNotFound ndMap edge.edgeSrc,
            mappedNodeNF onNotFound ndMap edge.edgeDst)
        with
        | Some src, Some dst -> Some { edgeSrc = src ; edgeDst = dst }
        | None,_ | _,None -> None
        
let remapNodeWithNF notfound ndMap nd =
    nd.nodeInEdges <- List.map_option (mapEdgeNodes notfound ndMap)
        nd.nodeInEdges ;
    nd.nodeOutEdges <- List.map_option (mapEdgeNodes notfound ndMap)
        nd.nodeOutEdges

let remapNode = remapNodeWithNF (fun x -> Some x)
let remapDiscardNode = remapNodeWithNF (fun _ -> None)
    
let remapIndices ndMap map =
    NodeMap.fold (fun key v cur -> NodeMap.add
        (NodeMap.find key ndMap) v cur) map NodeMap.empty
        
let remapIndicesDiscard ndMap map =
    NodeMap.fold (fun key v cur -> (try
            let mapped = NodeMap.find key ndMap in
            NodeMap.add mapped v cur
        with Not_found -> cur)) map NodeMap.empty

(** Copies a node. NOTE: the edges' other ends are left alike! *)
let copyNode nd =
    let nNd = newNode nd.nodeName in
    nNd.nodeInEdges <- nd.nodeInEdges ;
    nNd.nodeOutEdges <- nd.nodeOutEdges ;
    remapNode (NodeMap.singleton nd nNd) nNd ;
    nNd
    

(** Returns a map from events of the graph to new events, where the IDs were
    changed using the given function, and the edges and polarity were adapted
    accordingly. *)
let remapEspIndices indexer esp =
    let nMap,nSet = NodeSet.fold (fun nd (curMap,curSet) ->
            let nNd = { nd with nodeId = indexer nd.nodeId } in
            NodeMap.add nd nNd curMap, NodeSet.add nNd curSet)
        esp.evts (NodeMap.empty, NodeSet.empty) in
    NodeSet.iter (remapNode nMap) nSet ;
    let nPol = remapIndices nMap esp.pol in
    { evts = nSet ; pol = nPol }, nMap

(******** ESP *************************************************************)

let esp_empty = { evts = NodeSet.empty ; pol = NodeMap.empty }

(** WARNING! This *should always* remove any parallel information from
 the given esp. *)
let esp_copy_mapped esp =
    let remap = NodeSet.fold (fun nd cur ->
        NodeMap.add nd (copyNode nd) cur) esp.evts NodeMap.empty in
    NodeMap.iter (fun _ nNd -> remapNode remap nNd) remap ;
    let nEvts = NodeMap.fold (fun _ nNd cur -> NodeSet.add nNd cur)
        remap NodeSet.empty in
    let pol = NodeMap.fold (fun nd pol cur ->
        NodeMap.add (mappedNode remap nd) pol cur) esp.pol NodeMap.empty in
    { evts = nEvts; pol = pol }, remap
    
let esp_copy esp = fst @@ esp_copy_mapped esp

let esp_addNamedEvent name pol esp =
    let nNode = newNode name in
    nNode, {
        evts = NodeSet.add nNode esp.evts ;
        pol = NodeMap.add nNode pol esp.pol
    }
let esp_addEvent = esp_addNamedEvent ""

let esp_addEvents n pol esp =
    let rec doAdd lst esp = function
    | 0 -> lst,esp
    | n -> let nNode,nEsp = esp_addEvent pol esp in
        doAdd (nNode::lst) nEsp (n-1)
    in
    doAdd [] esp n

let esp_addNamedEvents names pol esp =
    Helpers.map2 List.rev Helpers.id @@
    List.fold_left (fun (cLst,cEsp) name ->
        let nNode,nEsp = esp_addNamedEvent name pol cEsp in
        ((name,nNode)::cLst),nEsp) ([],esp) names
        

let esp_addEdge n1 n2 =
    let edge = { edgeSrc = n1; edgeDst = n2 } in
    n1.nodeOutEdges <- edge::n1.nodeOutEdges ;
    n2.nodeInEdges <- edge::n2.nodeInEdges
    
(******** Games ***********************************************************)
let rec game_empty =
    { g_esp = esp_empty ; g_tree = None }
    
let game_unparallelize game = match game.g_tree with
| None -> game
| Some _ ->
    { g_esp = esp_copy game.g_esp ; g_tree = None }
        
let game_selfParallel game = match game.g_tree with
| None -> 
    { game with g_tree = Some(TreeLeaf(game)) }
| Some _ -> game
        
let mapRight fct (x,y) = x, fct y
let replRightEsp game pair =
    mapRight (fun e -> { game with g_esp = e }) pair
    
exception BadTreeStructure

let game_parallel_mapped g1 g2 =
    let pg1 = game_selfParallel g1
    and pg2 = game_selfParallel g2 in
    
    let pg1_tree,pg2_tree = match pg1.g_tree, pg2.g_tree with
        | None,_ | _,None -> raise BadTreeStructure
        | Some x,Some y -> x,y in
    
    (* If one of the games is empty, we simply return it as-is.
       It is sometimes helpful (e.g. during folds) to fold from the empty game
       without getting bothered by differences between empty ||| A and A.
    *)
    if NodeSet.is_empty g1.g_esp.evts then
        g2, NodeMap.empty, Helpers.selfNodeMap g2.g_esp.evts
    else if NodeSet.is_empty g2.g_esp.evts then
        g1, Helpers.selfNodeMap g1.g_esp.evts, NodeMap.empty
    else begin
        let nTree = Some (TreeNode(pg1_tree, pg2_tree)) in
        let remapG1 = NodeSet.fold (fun evt cur ->
                let wayId, ndId = match evt.nodeId with CompId(x,y) -> x,y in
                let nId = CompId(CompLeft(wayId), ndId) in
                NodeMap.add evt { evt with
                    nodeId = nId } cur)
            pg1.g_esp.evts NodeMap.empty in
        let remapG2 = NodeSet.fold (fun evt cur ->
                let wayId,ndId = match evt.nodeId with CompId(x,y) -> x,y in
                let nId = CompId(CompRight(wayId), ndId) in
                NodeMap.add evt { evt with
                    nodeId = nId } cur)
            pg2.g_esp.evts NodeMap.empty in
        
        let nEvts =
            let remapEvts map = NodeSet.fold (fun evt cur ->
                let nEvt = NodeMap.find evt map in
                remapNode map nEvt ;
                NodeSet.add nEvt cur) in
            remapEvts remapG2 pg2.g_esp.evts
                (remapEvts remapG1 pg1.g_esp.evts NodeSet.empty) in
        let nPols =
            let remapPols map = NodeMap.fold (fun evt pol cur ->
                    NodeMap.add (NodeMap.find evt map) pol cur) in
            remapPols remapG2 pg2.g_esp.pol
                (remapPols remapG1 pg1.g_esp.pol NodeMap.empty) in
        let nEsp = { evts = nEvts ; pol = nPols } in
        
        { g_esp = nEsp ; g_tree = nTree}, remapG1, remapG2
    end
    
let game_parallel g1 g2 = (fun (x,_,_) -> x) @@ game_parallel_mapped g1 g2

let game_copy_mapped game =
    let rec copyTree = function
    | TreeLeaf(gm) ->
        let nEsp,map = esp_copy_mapped game.g_esp in
        TreeLeaf({ g_esp = nEsp; g_tree = None }), map
    | TreeNode(lTree,rTree) ->
        let lNode,lMap = copyTree lTree in
        let rNode,rMap = copyTree rTree in
        let map = NodeMap.merge Helpers.mapMerger lMap rMap in
        TreeNode(lNode,rNode), map
    in
    
    let nTree, map =
        (match game.g_tree with
        | None ->
            None, snd @@ copyTree (TreeLeaf(game))
        | Some t -> (fun (x,y) -> Some x,y) @@ copyTree t) in
    
    let dag = NodeMap.fold (fun _ nd cur -> NodeSet.add nd cur) map
         NodeSet.empty in
    NodeSet.iter (remapNode map) dag ;
    let esp = { evts = dag ;
        pol = NodeMap.fold (fun key pol cur ->
            NodeMap.add (NodeMap.find key map) pol cur)
            game.g_esp.pol NodeMap.empty } in
    
    {
        g_esp = esp ;
        g_tree = nTree
    }, map
    
let game_copy game = fst @@ game_copy_mapped game

let game_addEvent pol game =
    let unpar = game_unparallelize game in
    replRightEsp unpar @@ esp_addEvent pol unpar.g_esp

let game_addNamedEvent name pol game =
    let unpar = game_unparallelize game in
    replRightEsp unpar @@ esp_addNamedEvent name pol unpar.g_esp
    
let game_addEvents nb pol game =
    let unpar = game_unparallelize game in
    replRightEsp unpar @@ esp_addEvents nb pol unpar.g_esp

let game_addNamedEvents names pol game =
    let unpar = game_unparallelize game in
    replRightEsp unpar @@ esp_addNamedEvents names pol unpar.g_esp
    
let game_addEdge = esp_addEdge

let treePair tree =
    match tree with
    | TreeLeaf(_) -> raise BadTreeStructure
    | TreeNode(l,r) -> l,r
let leftTree game = fst @@ treePair game
let rightTree game = snd @@ treePair game

let game_assocWithReindexer reindexer nTree game =
    let reindexNd nd =
        { nd with nodeId = reindexer nd.nodeId} in
    let reindexSet set =
        NodeSet.fold (fun nd (curSet,curMap) ->
            let nNd = reindexNd nd in
            NodeSet.add nNd curSet, NodeMap.add nd nNd curMap) set
        (NodeSet.empty,NodeMap.empty) in
    
    let nDag, nodeMapping = reindexSet game.g_esp.evts in
    NodeSet.iter (remapNode nodeMapping) nDag ;
    let nPol = remapIndices nodeMapping game.g_esp.pol in

    {
        g_esp = { evts = nDag ; pol = nPol } ;
        g_tree = Some nTree
    }, nodeMapping
    
    
let game_assocRight_mapped game =
    let gameTree = match game.g_tree with
        | None -> raise BadTreeStructure
        | Some t -> t in
    let gameA, gameB = treePair @@ leftTree gameTree in
    let gameC = rightTree gameTree in
    let nTree = TreeNode(gameA, TreeNode(gameB, gameC) ) in

    game_assocWithReindexer
        (function CompId(tree,gId) -> CompId((match tree with
        | CompLeft(CompLeft(x)) -> CompLeft(x)
        | CompLeft(CompRight(x)) -> CompRight(CompLeft(x))
        | CompLeft(CompBase) | CompBase -> raise BadTreeStructure
        | CompRight(x) -> CompRight(CompRight(x))), gId))
        nTree game
    
let game_assocLeft_mapped game =
    let gameTree = match game.g_tree with
        | None -> raise BadTreeStructure
        | Some t -> t in
    let gameA = leftTree gameTree in
    let gameB, gameC = treePair @@ rightTree gameTree in
    let nTree = TreeNode(TreeNode(gameA,gameB), gameC) in

    game_assocWithReindexer
        (function CompId(tree,gId) -> CompId((match tree with
        | CompRight(CompRight(x)) -> CompRight(x)
        | CompRight(CompLeft(x)) -> CompLeft(CompRight(x))
        | CompRight(CompBase) | CompBase -> raise BadTreeStructure
        | CompLeft(x) -> CompLeft(CompLeft(x))), gId))
        nTree game
        
let game_assocRight game = fst @@ game_assocRight_mapped game
let game_assocLeft game = fst @@ game_assocLeft_mapped game

type reassocTree = string Datatypes.binTreeStruct
exception BadReassocTree of reassocTree

type pathElem = PathLeft | PathRight
let game_reassoc_mapped game fromTree toTree =
    let rec mapTree mapper = function
    | TreeLeaf(x) -> mapper x
    | TreeNode(l,r) -> TreeNode(mapTree mapper l, mapTree mapper r)
    in
    
    (* Check whether the reallocation trees are correct *)
    let rec checkTreeLabels tree seenLabels = match tree with
    | TreeLeaf(lab) ->
        if SSet.mem lab seenLabels then
            raise @@ BadReassocTree tree;
        SSet.add lab seenLabels
    | TreeNode(l,r) ->
        let nSeen = checkTreeLabels l seenLabels in
        checkTreeLabels r nSeen
    in
    let fromLabels = checkTreeLabels fromTree SSet.empty in
    let toLabels = checkTreeLabels toTree SSet.empty in
    if not (SSet.subset fromLabels toLabels &&
            SSet.subset toLabels fromLabels)
        then raise @@ BadReassocTree toTree ;
    
    (* They are correct. Try to match fromTree with the game tree. *)
    let makePathRemapper () =
        let rec createDestMap map path = function
        | TreeLeaf(lab) -> SMap.add lab path map
        | TreeNode(l,r) ->
            let nMap = createDestMap map (PathLeft::path) l in
            createDestMap nMap (PathRight::path) r
        in
        let destMap = createDestMap SMap.empty [] toTree in
        let walkSourceTree = mapTree
            (fun lab -> TreeLeaf(SMap.find lab destMap)) in
        walkSourceTree fromTree
    in
    let rec walkTree2 map guideTree othTree = match guideTree,othTree with
    | TreeLeaf(lab), nd -> SMap.add lab nd map
    | TreeNode(guL,guR), TreeNode(otL,otR) ->
        let nMap = walkTree2 map guL otL in
        walkTree2 nMap guR otR
    | TreeNode _, TreeLeaf _ -> raise BadTreeStructure
    in
    let gameTree = match game.g_tree with
        | None -> raise BadTreeStructure
        | Some x -> x in
    let treeStructMap = walkTree2 SMap.empty fromTree gameTree in
    let pathRemapper = makePathRemapper () in
    
    (* Everything is mapped, we only have to create the new game now. *)
    let nTree = mapTree (fun lab -> SMap.find lab treeStructMap) toTree in
    let indexRemapper ind = match ind with CompId(way, numId) ->
        let rec replaceId remapTree way = match remapTree,way with
        | TreeNode(_), CompBase -> raise BadTreeStructure
        | TreeLeaf(path), wayRem ->
            List.fold_left (fun cur elt -> match elt with
                | PathLeft -> CompLeft(cur)
                | PathRight -> CompRight(cur)) wayRem path
        | TreeNode(tr,_), CompLeft(wayRem)
        | TreeNode(_,tr), CompRight(wayRem) -> replaceId tr wayRem
        in
        CompId(replaceId pathRemapper way, numId)
    in
    let nEsp,evtsMap = remapEspIndices indexRemapper game.g_esp in

    {
        g_tree = Some nTree ;
        g_esp = nEsp
    }, evtsMap

let game_reassoc game fromTree toTree =
    fst @@ game_reassoc_mapped game fromTree toTree
    
let game_extractOfId idTransformer nTree game =
    let nEvts, evtsMap = NodeSet.fold (fun nd (curEvts, curMap) ->
            (match idTransformer nd.nodeId with
            | None -> (curEvts, curMap)
            | Some nId ->
                let nNd = { nd with nodeId = nId } in
                NodeSet.add nNd curEvts, NodeMap.add nd nNd curMap))
        game.g_esp.evts (NodeSet.empty, NodeMap.empty) in
    
    NodeSet.iter (remapDiscardNode evtsMap) nEvts ;
    let nPol = remapIndicesDiscard evtsMap game.g_esp.pol in

    {
        g_esp = {
            evts = nEvts ;
            pol = nPol } ;
        g_tree = nTree
    }, evtsMap
    
let game_extractLeft_mapped game =
    let nTree = (match game.g_tree with
        | None | Some (TreeLeaf _) -> raise BadTreeStructure
        | Some (TreeNode(nTree, _)) -> Some nTree) in
    game_extractOfId
        (fun id -> (match id with
            | CompId(CompLeft(x), y) -> Some (CompId(x,y))
            | CompId(CompRight(_),_) -> None
            | CompId(CompBase,_) -> raise BadTreeStructure))
        nTree game

let game_extractRight_mapped game =
    let nTree = (match game.g_tree with
        | None | Some (TreeLeaf _) -> raise BadTreeStructure
        | Some (TreeNode(_,nTree)) -> Some nTree) in
    game_extractOfId
        (fun id -> (match id with
            | CompId(CompRight(x), y) -> Some (CompId(x,y))
            | CompId(CompLeft(_),_) -> None
            | CompId(CompBase,_) -> raise BadTreeStructure))
        nTree game
        
let game_extractLeft game = fst @@ game_extractLeft_mapped game
let game_extractRight game = fst @@ game_extractRight_mapped game

(*** Transitive reduction/closure ***)

let matrixOfGraph dag =
    let nbVert = NodeSet.cardinal dag in
    
    if nbVert = 0 then
        [||], NodeMap.empty, [||]
    else begin
        let indexer =
            let curId = ref 0 in
            (fun () -> incr curId;  !curId-1)
        in
        let vertOfId = Array.make nbVert (NodeSet.choose dag) in
        let idOfVert = NodeSet.fold (fun nd curDirect ->
                let id = indexer () in
                vertOfId.(id) <- nd ;
                NodeMap.add nd id curDirect)
            dag NodeMap.empty in
        
        let idOf vert =
            NodeMap.find vert idOfVert in

        let matr = Array.init nbVert (fun _ -> Array.make nbVert false) in
        
        NodeSet.iter (fun nd -> List.iter (fun edge ->
                (try
                    matr.(idOf edge.edgeSrc).(idOf edge.edgeDst) <- true
                with Not_found -> ())
                (* If the id is not found, that means that the vertice was not
                 * part of the considered graph. Thus, we might just ignore
                 * the edge. *)
            )
            nd.nodeOutEdges) dag ;
        
        matr, idOfVert, vertOfId
    end 

(** Removes all edges from [dag] and adds the edges defined by [matr]. *)
let applyEdgesMatrix dag matr vertOfId =
    let nodeOf id = vertOfId.(id) in (* Easier to understand. *)
    
    NodeSet.iter (fun nd -> nd.nodeInEdges <- []; nd.nodeOutEdges <- []) dag ;
    Array.iteri (fun fromId row -> Array.iteri (fun toId hasEdge ->
            if hasEdge then esp_addEdge (nodeOf fromId) (nodeOf toId))
        row) matr
        
let floydWarshall matr =
    let size = Array.length matr in
    assert (size = 0 || size = Array.length matr.(0)) ;
    for id = 0 to size-1 do
        matr.(id).(id) <- true
    done;
    for mid = 0 to size-1 do
        for dep = 0 to size-1 do
            for arr = 0 to size-1 do
                matr.(dep).(arr) <- matr.(dep).(arr) ||
                    (matr.(dep).(mid) && matr.(mid).(arr))
            done
        done
    done

let dag_transitiveClosure dag =
    let matr, _, vertOfId = matrixOfGraph dag in
    floydWarshall matr ;
    applyEdgesMatrix dag matr vertOfId

let dag_transitiveReduction dag =
    let pathMatr, _, vertOfId = matrixOfGraph dag in
    floydWarshall pathMatr ;
    
    let size = Array.length pathMatr in
    (* Reflexive reduction *)
    for id = 0 to size-1 do
        pathMatr.(id).(id) <- false
    done;
    for dep = 0 to size - 1 do
        for mid  = 0 to size - 1 do
            if pathMatr.(dep).(mid) then
                for arr = 0 to size - 1 do
                    if pathMatr.(mid).(arr) then
                        pathMatr.(dep).(arr) <- false
                done
        done
    done ;
    
    applyEdgesMatrix dag pathMatr vertOfId
                        
    
(******** Strategy ********************************************************)
    
    
let strat_copy strat =
    let nStrat,stratMap = esp_copy_mapped strat.st_strat in
    let map = NodeMap.fold (fun fromNd toNd cur -> NodeMap.add
            (mappedNode stratMap fromNd) toNd cur)
        strat.st_map NodeMap.empty in
    {
        st_strat = nStrat ;
        st_game = strat.st_game ;
        st_map = map
    }

let strat_new game = {
    st_strat = esp_empty ;
    st_game = game ;
    st_map = NodeMap.empty }

let strat_id game =
    let strat = strat_new game in
    let stratSt, stratIMap = esp_copy_mapped game.g_esp in
    let stratMap = NodeMap.fold (fun gEvt sEvt cur ->
        NodeMap.add sEvt gEvt cur) stratIMap NodeMap.empty in
    { strat with
        st_strat = stratSt ;
        st_map = stratMap
    }
    
let strat_addNamedEvent name evt strat =
    (*TODO check that the node is part of this game *)
    let nNode = newNode name in
    let nStrat = {
            evts = NodeSet.add nNode strat.st_strat.evts ;
            pol = NodeMap.add nNode
                (Helpers.getPolarity evt strat.st_game.g_esp)
                strat.st_strat.pol
        } in
    nNode, { strat with
        st_strat = nStrat ;
        st_map = NodeMap.add nNode evt strat.st_map
    }
    
let strat_addEvent = strat_addNamedEvent ""

let strat_addNamedEvents nEvts strat =
    Helpers.map2 List.rev Helpers.id @@
    List.fold_left (fun (cOut, cStrat) (evtName,evt) ->
        let nEvt,nStrat = strat_addNamedEvent evtName evt cStrat in
        (evtName,nEvt) :: cOut, nStrat) ([],strat) nEvts
        
let strat_addEvents nEvts strat =
    let lst,nStrat = strat_addNamedEvents
        (List.map (fun x -> ("",x)) nEvts) strat in
    (List.map (fun (_,nd) -> nd) lst),nStrat
        
let strat_addEdge = esp_addEdge

let strat_newFilled_namemapped game =
    let strat = strat_new game in
    let nameMap, evMap, nStrat = NodeSet.fold
        (fun nd (nameMap, curMap,curStrat) ->
            let nEvt,nStrat = strat_addNamedEvent nd.nodeName nd curStrat in
            SMap.add nd.nodeName nEvt nameMap,
                NodeMap.add nd nEvt curMap,
                nStrat
        ) game.g_esp.evts (SMap.empty, NodeMap.empty, strat) in
    NodeSet.iter (fun nd -> List.iter (fun edge ->
        (match mapEdgeNodes (fun x -> Some x) evMap edge with
            | Some edge -> strat_addEdge edge.edgeSrc edge.edgeDst
            | None -> ()
        )) nd.nodeOutEdges)
        game.g_esp.evts ;

    nStrat, nameMap
    
let strat_newFilled game =
    fst @@ strat_newFilled_namemapped game
    
let strat_assocRight strat =
    let nGame, map = game_assocRight_mapped strat.st_game in
    let nMap = NodeMap.map (mappedNode map) strat.st_map in
    { strat with
        st_game = nGame; st_map = nMap }

let strat_assocLeft strat =
    let nGame, map = game_assocLeft_mapped strat.st_game in
    let nMap = NodeMap.map (mappedNode map) strat.st_map in
    { strat with
        st_game = nGame; st_map = nMap }
    
let strat_reassoc strat fromTree toTree =
    let nGame, gameMap = game_reassoc_mapped strat.st_game fromTree toTree in
    let nMap = NodeMap.map (mappedNode gameMap) strat.st_map in
    { strat with
        st_game = nGame; st_map = nMap }
    
let strat_extractWith extractor strat =
    let nGame, map = extractor strat.st_game in
    let nMap,nEvts = NodeMap.fold (fun sNd gNd (curMap, curEvts) ->
            (try
                NodeMap.add sNd (NodeMap.find gNd map) curMap,
                NodeSet.add sNd curEvts
            with Not_found -> curMap,curEvts)
        ) strat.st_map (NodeMap.empty,NodeSet.empty) in
    {
        st_strat = { strat.st_strat with evts = nEvts };
        st_map = nMap ;
        st_game = nGame
    }
    
let strat_extractLeft = strat_extractWith game_extractLeft_mapped    
let strat_extractRight = strat_extractWith game_extractRight_mapped    

