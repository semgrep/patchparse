open Ce
open Ce_visit
open Ast_visit

(* ---------------------------------------------------------------------- *)
(* get characters, for equivalence class creation. *)

let get_characters ce =
  let rec process_prim x = function
      Ast.IDENT(x,_) -> Some [x]
    | Ast.EXP(_,Some exp) -> process_prim x exp
    | _ -> None in
  let process_expr _ _ = None in
  let process_code _ _ = None in
  let (prim,expr,code) =
    process_term (List.fold_left Aux.union [])
      process_prim process_expr process_code in
  (* inefficient because we collect both components, but only use one *)
  process_ce (function (x,y) -> x)
    (List.fold_left Aux.union []) prim expr code ce

(* ---------------------------------------------------------------------- *)
(* get function names, for protocol detection. *)

let get_function_names ce =
  let process_prim _ _ = None in
  let process_expr (do_prim,do_code) = function
    (* no declarer here - not a protocol *)
      Ast.CALL(fn,args,Ast.FRONTUNKNOWN) -> None
    | Ast.CALL(fn,args,known) ->
      Some
        (List.fold_left Aux.union []
           (get_characters (EXPRCE(fn,fn)) :: List.map do_code args))
    | _ -> None in
  let process_code _ _ = None in
  let (prim,expr,code) =
    process_term (List.fold_left Aux.union [])
      process_prim process_expr process_code in
  process_ce (function (x,y) -> (x,y))
    (List.fold_left Aux.union []) prim expr code ce

(* ---------------------------------------------------------------------- *)

let function_change ce =
  let process_prim _ _ = None in
  let process_expr _ = function
      Ast.CALL(_,_,Ast.KNOWN) | Ast.CALL(_,_,Ast.ENDUNKNOWN) -> Some true
    | Ast.DECLARER(_,_,Ast.KNOWN) (* not sure if this should be here *)
    | Ast.DECLARER(_,_,Ast.ENDUNKNOWN) -> Some true
    | _ -> None in
  let process_code _ _ = None in
  let exists = List.exists (function x -> x) in
  let (prim,expr,code) =
    process_term exists process_prim process_expr process_code in
  process_ce (function (x,y) -> x || y) exists prim expr code ce
