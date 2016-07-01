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

open LlamAst
open Datatypes
open Operations.Canonical

exception UnboundVar of lamVar
exception BadTyping of lamTerm
exception NonLinearTerm

let findType env v =
    (try SMap.find v env
    with Not_found -> raise @@ UnboundVar v)
    
let rec expandType env typ = match typ with
| LamAtom(t) ->
    (try expandType env (SMap.find t env)
    with Not_found -> typ)
| LamArrow(l,r) -> LamArrow(expandType env l, expandType env r)

let rec typeOf env term = match term with
| LamVar v -> findType env v
| LamApp(s,t) ->
    let lType = typeOf env s in
    let rType = typeOf env t in
    (match lType with
    | LamArrow(fromTyp,toTyp) ->
        if fromTyp = rType then
            toTyp
        else
            raise @@ BadTyping(term)
    | _ -> raise @@ BadTyping(term))
| LamAbs(v, vTyp, absTerm) ->
    LamArrow(vTyp, typeOf (SMap.add v vTyp env) absTerm)

let typeOfTerm term = typeOf SMap.empty term
    
let gameOfType =
    let gameEnv = ref SMap.empty in
    (fun envList typ ->
        let rec mkGame typ = Builder.(Operations.Canonical.(match typ with
        | LamAtom(atom) ->
            if not @@ SMap.mem atom !gameEnv then
                gameEnv := SMap.add atom
                    (snd @@ game_addNamedEvent atom PolNeg game_empty)
                    !gameEnv ;
            SMap.find atom !gameEnv
        | LamArrow(l,r) ->
            let lGame = perp @@ mkGame l in
            let rGame = mkGame r in
            lGame |||: rGame
        )) in
        let rec mkEnv envList = Builder.(Operations.Canonical.(
            match envList with
            | [] -> game_empty
            | (_,typ)::[] -> perp @@ mkGame typ
            | (_,typ)::tl ->
                (mkEnv tl) |||: (perp @@ mkGame typ)))
        in
        (mkEnv envList) |||: (mkGame typ)
    )

let gameOfTerm env term =
    gameOfType env @@ typeOfTerm term

let splitEnv env lTerm rTerm =
    (* Returns (outEnv, digEnv \ {extracted}, alreadyExtracted u {extracted})*)
    let rec digEnv curEnv toSplit alreadyExtracted unwatched = function
    | LamVar(v) ->
        if List.mem_assoc v toSplit then (* Extract from environment *)
            (v,(List.assoc v toSplit))::curEnv,
                List.remove_assoc v toSplit,
                SSet.add v alreadyExtracted
        else if SSet.mem v alreadyExtracted then
            raise NonLinearTerm
        else if not @@ SSet.mem v unwatched then
            raise @@ UnboundVar v
        else
            curEnv, toSplit, alreadyExtracted
    | LamAbs(v,_,absTerm) ->
        digEnv curEnv toSplit alreadyExtracted (SSet.add v unwatched) absTerm
    | LamApp(lTerm,rTerm) ->
        let nEnv, nToSplit, nExtracted =
            digEnv curEnv toSplit alreadyExtracted unwatched lTerm in
        digEnv nEnv nToSplit nExtracted unwatched rTerm
    in
    
    let lEnv, nEnv, extracted =
        digEnv [] env SSet.empty SSet.empty lTerm in
    let rEnv, finalEnv, _ =
        digEnv [] nEnv extracted SSet.empty rTerm in
    if finalEnv <> [] then
        raise NonLinearTerm ;
    lEnv,rEnv

let stratOfTerm term = Builder.(
    let rec doBuild  listEnv env term = match term with
    | LamVar v ->
        (match listEnv with
        | (var,_)::[] -> assert(v=var)
        | _ -> assert false) ;
        copycat (gameOfType [] @@ findType env v)
    | LamAbs(v,vTyp,absTerm) ->
        (if listEnv = []
            then (fun x -> x)
            else strat_assocRight) @@
            doBuild ((v,vTyp)::listEnv) (SMap.add v vTyp env) absTerm
    | LamApp(lTerm,rTerm) ->
        let lListEnv,rListEnv = splitEnv listEnv lTerm rTerm in
        let lEnv = List.fold_left (fun cur (v,typ) -> SMap.add v typ cur)
            SMap.empty lListEnv in
        let rEnv = List.fold_left (fun cur (v,typ) -> SMap.add v typ cur)
            SMap.empty rListEnv in

        let ccStrat = copycat
            (gameOfType [] @@ typeOf env lTerm) in
        
        let parStrat = 
            let mkEnvOrder =
                let rec idOf name cPos = function
                | [] -> raise Not_found
                | (hd,_)::tl ->
                    if name = hd
                        then cPos
                        else idOf name (cPos+1) tl
                in
                List.map (fun (name,_) -> idOf name 0 listEnv)
            in
            let lEnvOrder = mkEnvOrder lListEnv
            and rEnvOrder = mkEnvOrder rListEnv in
            
            let rec mkReassocTree labels =
                let mkLabel pos = TreeLeaf(string_of_int pos) in
                match labels with
                | [] -> assert false
                | pos::[] -> mkLabel pos
                | pos::tl -> TreeNode(mkReassocTree tl, mkLabel pos)
            in
            let rec mkFinalTree cId = function
            | 0 -> assert false
            | 1 -> TreeLeaf(string_of_int cId)
            | k -> TreeNode(mkFinalTree (cId+1) (k-1),
                TreeLeaf(string_of_int cId)) in
            
            let lStrat = doBuild lListEnv lEnv lTerm
            and rStrat = doBuild rListEnv rEnv rTerm in
            let parStrat = strat_reassoc (lStrat ||| rStrat)
                (TreeNode(
                    TreeNode(TreeLeaf("gamma"),TreeLeaf("A")),
                    TreeNode(TreeLeaf("delta"),TreeLeaf("B"))))
                (TreeNode(
                    TreeNode(TreeLeaf("gamma"),TreeLeaf("delta")),
                    TreeNode(TreeLeaf("A"),TreeLeaf("B")))) in
            
            let reassocTree,finalTree = match lEnvOrder,rEnvOrder with
            | [],[] ->
                TreeLeaf("A"),TreeLeaf("A")
            | onlyEnv,[] | [],onlyEnv ->
                TreeNode(mkReassocTree onlyEnv, TreeLeaf("right")),
                TreeNode(mkFinalTree 0 (List.length listEnv),
                    TreeLeaf("right"))
            | _,_ ->
                TreeNode(TreeNode(
                    mkReassocTree lEnvOrder,
                    mkReassocTree rEnvOrder), TreeLeaf("right")),
                TreeNode(mkFinalTree 0 (List.length listEnv),
                    TreeLeaf("right")) in
            
            strat_reassoc parStrat reassocTree finalTree in
        
            
        (*
        Printer.dispDebugStrategy ccStrat ;
        Printer.dispDebugStrategy parStrat ; *)
        ccStrat @@@ parStrat
    in
    doBuild [] SMap.empty (LlamHelpers.disambiguate term)
)

