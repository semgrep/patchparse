open Ast

let process_term combiner process_prim process_expr process_code =
  let rec do_prim prim =
    match process_prim (do_expr,do_code) prim with
      Some x -> x
    | None ->
	(match prim with
	  ARRAY(exprlist,known) -> combiner (List.map do_expr exprlist)
	| PARENSYM(exprlist,known) -> combiner (List.map do_expr exprlist)
	| _ -> combiner [])
  and do_expr expr =
    match process_expr (do_prim,do_code) expr with
      Some x -> x
    | None ->
	(match expr with
	  SYMBOL(sym) -> combiner (List.map do_prim sym)
	| ASSIGN(lhs,op,rhs,FRONTUNKNOWN) ->
	    combiner (List.map do_expr rhs)
	| ASSIGN(lhs,op,rhs,known) ->
	    combiner (do_expr lhs :: List.map do_expr rhs)
	| CALL(fn,args,FRONTUNKNOWN)
	| DECLARER(fn,args,FRONTUNKNOWN) ->
	    combiner (List.map do_code args)
	| CALL(fn,args,known)
	| DECLARER(fn,args,known)->
	    combiner (do_expr fn :: List.map do_code args)
	| PROTOTYPE(fn,_,_,_,params,known) ->
	    combiner (do_expr fn :: List.map do_code params)
	| STRUCT(fields,known) -> combiner (List.map do_code fields)
	| _ -> combiner [])
  and do_code code =
    match process_code (do_prim,do_expr) code with
      Some x -> x
    | None ->
	(match code with
	  EXPR(exprs) -> combiner (List.map do_expr exprs)
	| _ -> combiner []) in
  (do_prim,do_expr,do_code)

let rebuild_term rebuild_prim rebuild_expr rebuild_code =
  let rec do_prim prim =
    match rebuild_prim do_prim do_expr do_code prim with
      Some x -> x
    | None ->
	(match prim with
	  ARRAY(exprlist,known) ->
	    ARRAY(List.map do_expr exprlist,known)
	| PARENSYM(exprlist,known) ->
	    PARENSYM(List.map do_expr exprlist,known)
	| _ -> prim)
  and do_expr expr =
    match rebuild_expr do_prim do_expr do_code expr with
      Some x -> x
    | None ->
	(match expr with
	  SYMBOL(sym) -> SYMBOL(List.map do_prim sym)
	| ASSIGN(lhs,op,rhs,FRONTUNKNOWN) ->
	    ASSIGN(lhs,op,List.map do_expr rhs,FRONTUNKNOWN)
	| ASSIGN(lhs,op,rhs,known) ->
	    let lhs = do_expr lhs in
	    let rhs = List.map do_expr rhs in
	    ASSIGN(lhs,op,rhs,known)
	| CALL(fn,args,FRONTUNKNOWN) ->
	    CALL(fn,List.map do_code args,FRONTUNKNOWN)
	| CALL(fn,args,known) ->
	    let fn = do_expr fn in
	    let args = List.map do_code args in
	    CALL(fn,args,known)
	| DECLARER(fn,args,FRONTUNKNOWN) ->
	    DECLARER(fn,List.map do_code args,FRONTUNKNOWN)
	| DECLARER(fn,args,known) ->
	    let fn = do_expr fn in
	    let args = List.map do_code args in
	    DECLARER(fn,args,known)
	| PROTOTYPE(fn,x,y,z,params,known) ->
	    let fn = do_expr fn in
	    let params = List.map do_code params in
	    PROTOTYPE(fn,x,y,z,params,known)
	| STRUCT(fields,known) ->
	    STRUCT(List.map do_code fields,known)
	| _ -> expr)
  and do_code code =
    match rebuild_code do_prim do_expr do_code code with
      Some x -> x
    | None ->
	(match code with
	  EXPR(exprs) -> EXPR(List.map do_expr exprs)
	| _ -> code) in
  (do_prim,do_expr,do_code)
