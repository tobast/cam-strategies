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

let getPolarity nd esp =
    (try NodeMap.find nd esp.pol
    with Not_found -> raise InvalidStrategy)
    
let getGameNode nd strat =
    (try NodeMap.find nd strat.st_map
    with Not_found -> raise InvalidStrategy)
    
let nextId =
    let cId = ref (-1) in (* -1: will be incremented on first use. *)
    (fun () -> incr cId; !cId)
    
let map2 f1 f2 (x,y) =
    f1 x, f2 y
let id x = x

exception MergeConflict
let mapMerger _ x y = match (x,y) with
| Some a, Some b -> if a = b then Some a else raise MergeConflict
| None, Some x | Some x, None -> Some x
| None,None -> None

let esp_eventsEquality e1 e2 = e1.evts = e2.evts

let rec treesEqualityNoPol t1 t2 = (match t1,t2 with
| TreeLeaf(lg1), TreeLeaf(lg2) ->
        esp_eventsEquality lg1.g_esp lg2.g_esp
| TreeNode(lt1,rt1), TreeNode(lt2,rt2) ->
        (treesEqualityNoPol lt1 lt2) && (treesEqualityNoPol rt1 rt2)
| TreeLeaf _, TreeNode _ | TreeNode _, TreeLeaf _ -> false)

let gamesEqualityNoPol g1 g2 = match g1.g_tree, g2.g_tree with
| None, None -> esp_eventsEquality g1.g_esp g2.g_esp
| Some t1, Some t2 -> treesEqualityNoPol t1 t2
| None, Some _ | Some _, None -> false
            
let gameIn g gSuper =
    let rec gameInTree = function
    | TreeLeaf(treeGame) -> gamesEqualityNoPol g treeGame
    | TreeNode(lt1,lt2) -> gameInTree lt1 || gameInTree lt2
    in
    (match gSuper.g_tree with
    | None -> gamesEqualityNoPol g gSuper
    | Some tree -> gameInTree tree)

let selfNodeMap elts =
    NodeSet.fold (fun elt cur -> NodeMap.add elt elt cur) elts NodeMap.empty

let eventsEqual e1 e2 =
    getBaseId e1.nodeId = getBaseId e2.nodeId

let rec dispWay fmt way =
    let wayChr = function
    | CompBase -> ""
    | CompLeft _ -> "←"
    | CompRight _ -> "→"
    in match way with
    | CompBase -> ()
    | CompLeft(tl) | CompRight(tl) ->
            Format.fprintf fmt "%s" (wayChr way) ; dispWay fmt tl

let dumpTreeWith dumper fmt tree =
    let rec doDump = function
        | TreeLeaf leaf ->
                Format.fprintf fmt " o %a@]" dumper leaf
        | TreeNode(l,r) ->
                Format.fprintf fmt "@,|—@[<v 1>";
                doDump l ;
                Format.fprintf fmt "@,|—@[<v 1>";
                doDump r ;
                Format.fprintf fmt "@]"
    in
    Format.fprintf fmt "@[<v 0>" ;
    doDump tree ;
    Format.fprintf fmt "@."
    
let dumpTreeStructure fmt tree = dumpTreeWith
    (fun _ _ -> ()) fmt tree
    
let dumpGameTree fmt tree =
    let dispPol fmt = function
        | PolPos -> Format.fprintf fmt "+"
        | PolNeg -> Format.fprintf fmt "-"
        | PolNeutral -> () in
    dumpTreeWith (fun fmt game ->
        let pickedName,pickedPol = (try
                let pickNd=NodeSet.choose game.g_esp.evts in
                pickNd.nodeName, (NodeMap.find pickNd game.g_esp.pol)
            with Not_found -> "ø",PolNeutral) in
        Format.fprintf fmt " %s (%a)" pickedName dispPol pickedPol)
        fmt tree
    
