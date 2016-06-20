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

module Array = struct
    include Array
    
    let fold_left_i worker start arr =
        let size = Array.length arr in
        let rec doFold cur pos =
            if pos = size
                then cur
                else doFold (worker pos cur arr.(pos)) (pos+1)
        in doFold start 0
        
    let exists pred =
        Array.fold_left (fun cur x -> cur || (pred x)) false
end

