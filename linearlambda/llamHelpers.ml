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
open LlamAst

module GVSet = Set.Make(struct type t=lccsVar let compare=compare end)
module GVMap = Map.Make(struct type t=lccsVar let compare=compare end)

let lambdaize str =
    LlamParser.term LlamLexer.token (Lexing.from_string str)

let disambiguate term =
    let addQuote var = match var with
    | StrVar(x) -> StrVar(x^"'")
    | ChVar(CcsCh(x)) -> ChVar(CcsCh(x^"'"))
    | ChVar(CcsOppCh(x)) -> ChVar(CcsOppCh(x^"'"))
    in
    let unwrapChan = function
    | ChVar(x) -> x
    | _ -> assert false
    in
    let rec newName var seen = match GVSet.mem var seen with
    | true -> newName (addQuote var) seen
    | false -> var
    in

    let rec doDisambiguate renameEnv seen = function
    | CcsZero -> CcsZero, seen
    | CcsOne -> CcsOne, seen
    | CcsParallel(lTerm, rTerm) ->
        let nlTerm, nlSeen = doDisambiguate renameEnv seen lTerm in
        let nrTerm, nSeen = doDisambiguate renameEnv nlSeen rTerm in
        CcsParallel(lTerm, rTerm), nSeen
    | CcsSeq(lTerm, rTerm) ->
        let nlTerm, nlSeen = doDisambiguate renameEnv seen lTerm in
        let nrTerm, nSeen = doDisambiguate renameEnv nlSeen rTerm in
        CcsSeq(lTerm, rTerm), nSeen
    | CcsCallChan(chan, term) ->
        let nName = GVMap.find (ChVar chan) renameEnv in
        let nTerm, nSeen = doDisambiguate renameEnv seen term in
        CcsCallChan(unwrapChan nName, nTerm), nSeen
    | CcsNew(chan, term) ->
        let nName = newName (ChVar chan) seen in
        let nTerm, nSeen = doDisambiguate
            (GVMap.add (ChVar chan) nName renameEnv)
            (GVSet.add nName seen) term in
        CcsNew(unwrapChan nName, nTerm), nSeen
    | LamTensor(lTerm, rTerm) ->
        let nlTerm, nlSeen = doDisambiguate renameEnv seen lTerm in
        let nrTerm, nSeen = doDisambiguate renameEnv nlSeen rTerm in
        LamTensor(lTerm, rTerm), nSeen
    | LamVar(v) -> LamVar(GVMap.find v renameEnv),seen
    | LamAbs(v,vTyp,term) ->
        let nName = newName v seen in
        let nTerm, nSeen = doDisambiguate (GVMap.add v nName renameEnv)
            (GVSet.add nName seen) term in
        (LamAbs(nName,vTyp,nTerm), nSeen)
    | LamApp(lTerm, rTerm) ->
        let nlTerm, mSeen = doDisambiguate renameEnv seen lTerm in
        let nrTerm, nSeen = doDisambiguate renameEnv mSeen rTerm in
        (LamApp(nlTerm, nrTerm), nSeen)
    in
    fst @@ doDisambiguate GVMap.empty GVSet.empty term

let oppCh = function
| CcsCh(v) -> CcsOppCh(v)
| CcsOppCh(v) -> CcsCh(v)
