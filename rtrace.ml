(******************************************************************************
 *        
 * REWRITELIB                        
 *                                   
 * trace.ml                       
 *                               
 * This file contains hooks for tracing. 
 *                               
 * (C) 2017, Kenneth Roe      
 *  
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 * will be provided when the work is complete.
 *
 * For a commercial license, contact Roe Mobile Development, LLC at
 * info@roemobiledevelopment.com
 *  
 *****************************************************************************)

(* require "trace-s.sml" ;  *)
(* require "list.sml" ;  *)
(* require "basis.__text_io" ;  *)

(* open listimpl ; *)
(* open TextIO ; *)

 let f = ref (fun _ -> ()) ;;

 let trace_on = ref false ;;

 let is_trace_on _ = (!trace_on) ;;

 let toggle_trace () =
     (if !trace_on then
          trace_on := false
     else
          trace_on := true)

(*val blocks = ref ["rewriteRule","rewriteRuleDisc","Disc","match","rewrite","derive"] ;*)
let blocks = ref ["match";"rewriteRuleDisc";"rewriteRule";"kbrewrite";"rewrite";"derive";"kbrewrite"] ;;
(*val blocks = ref ["env"] ;*)

let setBlocks bl = (blocks := bl);;

let ind = ref 0 ;;

let indent () = (ind := (!ind)+2) ;;
let undent () = (ind := (!ind)-2) ;;

let rec indent_line n = if n <> 0 then !f (String.make n ' ')

let trace x s =
     if !trace_on && List.mem x (!blocks) then
         (indent_line (!ind) ;
          (*print_string ((s ()) ^ "\n") ; flush stdout ;*)
          (!f) ((s ()) ^ "\n")) else () ;;

let trace_list x s =
     if List.mem x (!blocks) && (!trace_on) then
         (List.iter
             (fun (x) ->
                 (indent_line (!ind) ;
                  (*print_string (x ^ "\n") ; flush stdout ;*)
                  (!f) (x ^ "\n"))) (s ())
     )
     else () ;;

let set_traceout fn = f := fn ;;
