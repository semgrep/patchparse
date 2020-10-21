open Change
open Ce_visit
open Ast_visit

(* ---------------------------------------------------------------------- *)
(* filtering functions *)

(* In some cases these are just boring, such as calls to dbg and err.  In
   other cases, they do not contain enough information to determine whether
   similar things are related or not, such as return, break, and continue. *)
let (boring_prim,boring_expr,boring_code,boring_ce) =
  let process_prim _ = function
      Ast.IDENT(string,_) -> Some (Config2.boring_ids string)
    | _ -> None in
  let process_expr _ _ = None in
  let process_code _ _ = None in
  let (boring_prim,boring_expr,boring_code) =
    process_term (List.for_all (function x -> x))
      process_prim process_expr process_code in
  let default =
    process_ce (function (x,y) -> x && y) (List.for_all (function x -> x))
      boring_prim boring_expr boring_code in
  (boring_prim,boring_expr,boring_code,
   function
     EXPRCE(Ast.CALL(f1,x,y),Ast.CALL(f2,_,_))
     when not (Ast_al.al_expr f1 = Ast_al.al_expr f2) ->
     (* idea: converting from one boring function to another is actually
        	 interesting *)
     false
   | (CODECE(Ast.EXPR(e1),Ast.EXPR(e2))) as ce -> default ce
   | CODECE(c1,c2) -> false
   | ce -> default ce)

let boring_symbol = List.for_all boring_prim
let boring_expr_list = List.for_all boring_expr
let boring_code_list = List.for_all boring_code
