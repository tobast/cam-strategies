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

let lambdaize str =
    LlamParser.term LlamLexer.token (Lexing.from_string str)

let disambiguate term =
    let rec newName var seen = match SSet.mem var seen with
    | true -> newName (var^"'") seen
    | false -> var
    in
    
    let rec doDisambiguate renameEnv seen = function
    | LamVar(v) -> LamVar(SMap.find v renameEnv),seen
    | LamAbs(v,vTyp,term) ->
        let nName = newName v seen in
        let nTerm, nSeen = doDisambiguate (SMap.add v nName renameEnv)
            (SSet.add nName seen) term in
        (LamAbs(nName,vTyp,nTerm), nSeen)
    | LamApp(lTerm, rTerm) ->
        let nlTerm, mSeen = doDisambiguate renameEnv seen lTerm in
        let nrTerm, nSeen = doDisambiguate renameEnv mSeen rTerm in
        (LamApp(nlTerm, nrTerm), nSeen)
    in
    fst @@ doDisambiguate SMap.empty SSet.empty term
