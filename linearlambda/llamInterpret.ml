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
open LlamHelpers
open Datatypes
open Operations.Canonical

exception UnboundVar of lccsVar
exception BadTyping of lamTerm
exception NonLinearTerm
exception NotImplemented

let findType env v =
    (try GVMap.find v env
    with Not_found -> raise @@ UnboundVar v)

let nameOfVar v = match v with
| StrVar(x) -> x
| ChVar(CcsCh(a)) -> a
| ChVar(CcsOppCh(a)) -> "_"^a

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
    LamArrow(vTyp, typeOf (GVMap.add v vTyp env) absTerm)
| LamTensor(l,r) ->
    LamTensorType(typeOf env l, typeOf env r)
| CcsZero -> CcsProg
| CcsOne -> CcsProg
| CcsCallChan(ch, prg) ->
    (match (findType env (ChVar ch)) with
    | CcsChan ->
        let nEnv = GVMap.remove (ChVar ch) env in
        typeOf nEnv prg
    | _ -> raise @@ BadTyping term)
| CcsParallel(l,r) ->
    (match (typeOf env l, typeOf env r) with
    | CcsProg,CcsProg -> CcsProg
    | _,_ -> raise @@ BadTyping(term))
| CcsSeq(l,r) ->
    (match (typeOf env l, typeOf env r) with
    | CcsProg,CcsProg | CcsChan,CcsProg -> CcsProg
    | _,_ -> raise @@ BadTyping(term))
| CcsNew(v,t) ->
    let nEnv = GVMap.add (ChVar(LlamHelpers.oppCh v)) CcsChan
        (GVMap.add (ChVar(v)) CcsChan env) in
    typeOf nEnv t

let typeOfTerm term = typeOf GVMap.empty term

let progGame, progGameCall, progGameDone, chanGame, chanGameCall, chanGameDone=
    let mkBaseGame prefix = Builder.(
        let evCall,g1 = game_addNamedEvent (prefix^"call") PolNeg game_empty in
        let evDone,g2 = game_addNamedEvent (prefix^"done") PolPos g1 in
        game_addEdge evCall evDone ;
        g2, evCall,evDone)
        in
    let progGame, pgCall, pgDone = mkBaseGame "P" in
    let chanGame, cgCall, cgDone = mkBaseGame "C" in
    progGame, pgCall, pgDone, chanGame, cgCall, cgDone

let gameOfType =
    (fun envList typ ->
        let rec mkGame typ = Builder.(Operations.Canonical.(match typ with
        | CcsProg -> progGame
        | CcsChan -> chanGame
        | LamTensorType(l,r) ->
            (mkGame l) |||: (mkGame r)
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
        if envList = []
            then mkGame typ
            else (mkEnv envList) |||: (mkGame typ)
    )

let gameOfTerm env term =
    gameOfType env @@ typeOfTerm term

let splitEnv env lTerm rTerm =
    let consumeVar curEnv toSplit alreadyExtracted unwatched  v =
        if List.mem_assoc v toSplit then (* Extract from environment *)
            (v,(List.assoc v toSplit))::curEnv,
                List.remove_assoc v toSplit,
                GVSet.add v alreadyExtracted
        else if GVSet.mem v alreadyExtracted then
            raise NonLinearTerm
        else if not @@ GVSet.mem v unwatched then
            raise @@ UnboundVar v
        else
            curEnv, toSplit, alreadyExtracted
    in

    (* Returns (outEnv, digEnv \ {extracted}, alreadyExtracted u {extracted})*)
    let rec digEnv curEnv toSplit alreadyExtracted unwatched = function
    | LamVar(v) -> consumeVar curEnv toSplit alreadyExtracted unwatched v
    | LamAbs(v,_,absTerm) ->
        digEnv curEnv toSplit alreadyExtracted
            (GVSet.add v unwatched) absTerm
    | CcsParallel(lTerm,rTerm)
    | CcsSeq(lTerm, rTerm)
    | LamApp(lTerm,rTerm)
    | LamTensor(lTerm,rTerm) ->
        let nEnv, nToSplit, nExtracted =
            digEnv curEnv toSplit alreadyExtracted unwatched lTerm in
        digEnv nEnv nToSplit nExtracted unwatched rTerm
    | CcsOne
    | CcsZero -> curEnv, toSplit, alreadyExtracted
    | CcsCallChan(ch, term) ->
        let nEnv, nSplit, nAlreadyExtracted =
            consumeVar curEnv toSplit alreadyExtracted unwatched (ChVar ch) in
        digEnv nEnv nSplit nAlreadyExtracted unwatched term
    | CcsNew(ch, term) ->
        digEnv curEnv toSplit alreadyExtracted
            (GVSet.add (ChVar ch)
            (GVSet.add (ChVar (LlamHelpers.oppCh ch)) unwatched)) term
    in

    let lEnv, nEnv, extracted =
        digEnv [] env GVSet.empty GVSet.empty lTerm in
    let rEnv, finalEnv, _ =
        digEnv [] nEnv extracted GVSet.empty rTerm in
    if finalEnv <> [] then
        raise NonLinearTerm ;
    lEnv,rEnv

let envOfList = List.fold_left
    (fun cur (v,typ) -> GVMap.add v typ cur)
    GVMap.empty

let stratOfTerm term = Builder.(
    let rec doBuild  listEnv env nuChans term =
        let rmEnv = strat_extractRight in
        let addEnv strat =
            (** Adds an empty environment to strategies without environment. *)
            (strat_new game_empty) ||| strat
        in
        let parallelOfTwo lTerm rTerm lListEnv rListEnv =
            (** Creates the strategy corresponding to lTerm ||| rTerm, where
                both strategies are of the form TreeNode(env, strat),
                and reorders the strategies so that the output is of the form
                TreeNode(gEnv, lStrat ||| rStrat) with gEnv reordered to match
                the order in listEnv.
                Returns (the strategy described above, lName, rName)
            *)

            let lEnv = envOfList lListEnv
            and rEnv = envOfList rListEnv in

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

            let lStrat, lName = doBuild lListEnv lEnv nuChans lTerm
            and rStrat, rName = doBuild rListEnv rEnv nuChans rTerm in
            let parStrat = match lListEnv,rListEnv with
            | [],[] -> ((rmEnv lStrat) ||| (rmEnv rStrat))
            | _,[] ->
                strat_reassoc (lStrat ||| (rmEnv rStrat))
                    (TreeNode(
                        TreeNode(TreeLeaf("gamma"),TreeLeaf("A")),
                        TreeLeaf("B")))
                    (TreeNode(
                        TreeLeaf("gamma"),
                        TreeNode(TreeLeaf("A"),TreeLeaf("B"))))
            | [],_ ->
                strat_reassoc ((rmEnv lStrat) ||| rStrat)
                    (TreeNode(
                        TreeLeaf("A"),
                        TreeNode(TreeLeaf("gamma"),TreeLeaf("B"))))
                    (TreeNode(
                        TreeLeaf("gamma"),
                        TreeNode(TreeLeaf("A"),TreeLeaf("B"))))
            | _,_ ->
                strat_reassoc (lStrat ||| rStrat)
                (TreeNode(
                    TreeNode(TreeLeaf("gamma"),TreeLeaf("A")),
                    TreeNode(TreeLeaf("delta"),TreeLeaf("B"))))
                (TreeNode(
                    TreeNode(TreeLeaf("gamma"),TreeLeaf("delta")),
                    TreeNode(TreeLeaf("A"),TreeLeaf("B")))) in

            let reassocTree,finalTree = match lEnvOrder,rEnvOrder with
            | [],[] ->
                TreeLeaf("A"),TreeNode(TreeLeaf("EMPTY"),TreeLeaf("A"))
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

            strat_reassoc parStrat reassocTree finalTree, lName, rName in

    let insertInEnvironment strat var =
        (** Reassocs [strat], a strategy with shape [Gamma || (V || A)],
            into the shape [Gamma' || A], where [Gamma] is the environment
            described by the [listEnv] from which [var] is absent, and
            [Gamma'] is the same environment in which [V] has been inserted
            where [var] belongs. *)
        let rec combTree = function
        | [] -> assert false
        | hd::[] -> TreeLeaf(string_of_int hd)
        | hd::tl -> TreeNode(combTree tl, TreeLeaf(string_of_int hd))
        in
        let finalCombTree lst =
            let tree = combTree lst in
            TreeNode(tree, TreeLeaf("A"))
        in
        let rec seq curL fst last =
            if fst >= last
                then curL
                else seq ((last-1)::curL) fst (last-1)
        in
        let holeySeq last ign =
            seq (seq [] (ign+1) last) 0 ign in

        let posInList v lst =
            let rec doSearch pos = function
            | [] -> raise Not_found
            | hd::tl ->
                if (fst hd) = v
                    then pos
                    else doSearch (pos+1) tl
            in doSearch 0 lst
        in

        let varPos = posInList var listEnv in
        let envSize = List.length listEnv in

        if envSize = 1 then (* The environment is only the var *)
            strat_extractRight strat
        else
            let fromTree = TreeNode(combTree @@ holeySeq envSize varPos,
                TreeNode(TreeLeaf(string_of_int varPos), TreeLeaf("A"))) in
            let toTree = finalCombTree @@ seq [] 0 envSize in
            strat_reassoc strat fromTree toTree
    in

    let composeParallelWith lTerm rTerm cmpStrat assembleNames =
        let lListEnv,rListEnv = splitEnv listEnv lTerm rTerm in
        let parStrat, lName, rName =
            parallelOfTwo lTerm rTerm lListEnv rListEnv in

        (*
        let () =
            let disp = Printer.dispDebugStrategy in
            disp (cmpStrat lName rName) ;
            disp parStrat
            in
        *)
        (cmpStrat lName rName) @@@ parStrat, assembleNames lName rName
    in
    let concatAssembler mid a b = a ^ mid ^ b in

    let mkTriprogStrat () =
        let lprogStrat, lprogMap = strat_newFilled_mapped (perp progGame)
        and rprogStrat, rprogMap = strat_newFilled_mapped (perp progGame)
        and eprogStrat, eprogMap = strat_newFilled_mapped progGame in
        let cmp1Strat, l1Map, r1Map = lprogStrat |||~ rprogStrat in
        let fullStrat, llMap, e1Map = cmp1Strat |||~ eprogStrat in
        let lMap = Helpers.mapCompose
            (Helpers.mapCompose lprogMap l1Map) llMap in
        let rMap = Helpers.mapCompose
            (Helpers.mapCompose rprogMap r1Map) llMap in
        let eMap = Helpers.mapCompose eprogMap e1Map in
        fullStrat, lMap, rMap, eMap
    in

    match term with
    | CcsZero ->
        addEnv @@ snd @@ strat_addNamedEvent "Pcall" progGameCall @@
                strat_new progGame,
            "0"
    | CcsOne ->
        addEnv @@ strat_newFilled progGame, "1"
    | CcsParallel(lTerm, rTerm) ->
        let parStrategy lName rName =
            let fullStrat, lMap, rMap, eMap = mkTriprogStrat () in
            strat_addEdge
                (NodeMap.find progGameCall eMap)
                (NodeMap.find progGameCall lMap) ;
            strat_addEdge
                (NodeMap.find progGameCall eMap)
                (NodeMap.find progGameCall rMap) ;
            strat_addEdge
                (NodeMap.find progGameDone lMap)
                (NodeMap.find progGameDone eMap) ;
            strat_addEdge
                (NodeMap.find progGameDone rMap)
                (NodeMap.find progGameDone eMap) ;
            fullStrat
        in
        composeParallelWith lTerm rTerm parStrategy (concatAssembler " || ")
    | CcsSeq(lTerm, rTerm) ->
        let seqStrategy lName rName =
            let fullStrat, lMap, rMap, eMap = mkTriprogStrat () in
            strat_addEdge
                (NodeMap.find progGameCall eMap)
                (NodeMap.find progGameCall lMap) ;
            strat_addEdge
                (NodeMap.find progGameDone lMap)
                (NodeMap.find progGameCall rMap) ;
            strat_addEdge
                (NodeMap.find progGameDone rMap)
                (NodeMap.find progGameDone eMap) ;
            fullStrat
        in
        composeParallelWith lTerm rTerm seqStrategy (concatAssembler " ; ")
    | CcsCallChan(ch, term) ->
        let chanStrat, lpMap, chMap, eMap =
            let lprogStrat, lprogMap = strat_newFilled_mapped (perp progGame)
            and chanStrat, chanMap = strat_newFilled_mapped (perp chanGame)
            and eprogStrat, eprogMap = strat_newFilled_mapped progGame in

            let cmp1Strat, r1Map, e1Map = chanStrat |||~ eprogStrat in
            let fullStrat, l1Map, rrMap = lprogStrat |||~ cmp1Strat in
            let eMap = Helpers.mapCompose
                (Helpers.mapCompose eprogMap e1Map) rrMap in
            let rMap = Helpers.mapCompose
                (Helpers.mapCompose chanMap r1Map) rrMap in
            let lMap = Helpers.mapCompose lprogMap l1Map in
            fullStrat, lMap, rMap, eMap in
        strat_addEdge
            (NodeMap.find progGameCall eMap)
            (NodeMap.find chanGameCall chMap) ;
        strat_addEdge
            (NodeMap.find chanGameDone chMap)
            (NodeMap.find progGameCall lpMap) ;
        strat_addEdge
            (NodeMap.find progGameDone lpMap)
            (NodeMap.find progGameDone eMap) ;


        let termStrat, tName = doBuild
            (List.remove_assoc (ChVar ch) listEnv)
            (GVMap.remove (ChVar ch) env)
            nuChans term in

        let out = insertInEnvironment (chanStrat @@@ termStrat) (ChVar ch),
            Format.sprintf "[%s] - %s" (nameOfVar (ChVar ch)) tName in
        out

    | CcsNew(ch, term) ->
        let nuStrat =
            let lchanStrat, lchanMap = strat_newFilled_mapped chanGame
            and rchanStrat, rchanMap = strat_newFilled_mapped chanGame in
            let lprogStrat, lprogMap = strat_newFilled_mapped (perp progGame)
            and eprogStrat, eprogMap = strat_newFilled_mapped progGame in

            let chanStrat, l1chanMap, r1chanMap = lchanStrat |||~ rchanStrat in
            let lchMap = Helpers.mapCompose lchanMap l1chanMap
            and rchMap = Helpers.mapCompose rchanMap r1chanMap in

            let progStrat, l1progMap, e1progMap = lprogStrat |||~ eprogStrat in
            let lprMap = Helpers.mapCompose lprogMap l1progMap
            and eprMap = Helpers.mapCompose eprogMap e1progMap in

            strat_addEdge
                (NodeMap.find chanGameCall lchMap)
                (NodeMap.find chanGameDone rchMap) ;
            strat_addEdge
                (NodeMap.find chanGameCall rchMap)
                (NodeMap.find chanGameDone lchMap) ;
            strat_addEdge
                (NodeMap.find progGameCall eprMap)
                (NodeMap.find progGameCall lprMap) ;
            strat_addEdge
                (NodeMap.find progGameDone lprMap)
                (NodeMap.find progGameDone eprMap) ;

            let fullStrat = chanStrat ||| progStrat in
            let fromReassoc = TreeNode(
                TreeNode(TreeLeaf("C1"),TreeLeaf("C2")),
                TreeNode(TreeLeaf("P1"),TreeLeaf("PF"))) in
            let toReassoc = TreeNode(TreeNode(
                    TreeLeaf("C1"),
                    TreeNode(TreeLeaf("C2"),TreeLeaf("P1"))),
                TreeLeaf("PF")) in
            let outStrat = strat_reassoc fullStrat fromReassoc toReassoc in
            Builder.dag_transitiveReduction outStrat.st_strat.evts;
            outStrat in

        let termStrat, tName = doBuild
            ((ChVar(ch),CcsChan)::(ChVar(LlamHelpers.oppCh ch),CcsChan)::
                listEnv)
            (GVMap.add (ChVar ch) CcsChan (
                GVMap.add (ChVar (LlamHelpers.oppCh ch)) CcsChan env))
            nuChans term in

        let rotatedStrat =
            (if listEnv = []
                then (fun x -> (strat_new game_empty) ||| x)
                else strat_assocRight) @@
                strat_assocRight termStrat in

        nuStrat @@@ rotatedStrat,
            Format.sprintf "(ν[%s]) %s" (nameOfVar (ChVar ch)) tName
    | LamTensor(lTerm, rTerm) ->
        let lListEnv,rListEnv = splitEnv listEnv lTerm rTerm in
        let parStrat, lName, rName =
            parallelOfTwo lTerm rTerm lListEnv rListEnv in
        parStrat, (lName ^ " " ^ rName)
    | LamVar v ->
        (match listEnv with
        | (var,_)::[] -> assert(v=var)
        | _ -> assert false) ;
        let namer _ = function
        | CcLeft -> ((nameOfVar v)^" [env]")
        | CcRight -> (nameOfVar v)
        in
        copycat_named namer (gameOfType [] @@ findType env v),
            nameOfVar v
    | LamAbs(v,vTyp,absTerm) ->
        let absStrat, absName =
            doBuild ((v,vTyp)::listEnv) (GVMap.add v vTyp env)
                nuChans absTerm in
        (if listEnv = []
            then addEnv
            else strat_assocRight) absStrat,
        ("λ"^(nameOfVar v) ^"·"^absName)
    | LamApp(lTerm,rTerm) ->
        let ccStrat lName rName = strat_assocLeft @@
            copycat_named
                (fun nd _ -> match nd.nodeId with
                    | CompId(CompRight(_), _) -> lName ^ " " ^ rName
                    | CompId(CompLeft(CompLeft(_)),_) -> lName
                    | CompId(CompLeft(CompRight(_)),_) -> rName
                    | CompId(_) -> assert false)
                (gameOfType [] @@ typeOf env lTerm) in

        composeParallelWith lTerm rTerm ccStrat (concatAssembler " ")
    in

    fst @@ doBuild [] GVMap.empty GVSet.empty (LlamHelpers.disambiguate term)
)

