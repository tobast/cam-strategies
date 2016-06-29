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

type lamVar = string
type lamAtomic = string

type lamType =
    LamAtom of lamAtomic
  | LamArrow of lamType * lamType (* a -> b *)

type lamTerm =
    LamVar of lamVar                (* x *)
  | LamApp of lamTerm * lamTerm     (* M N *)
  | LamAbs of lamVar * lamType * lamTerm      (* λx : τ . M *)

