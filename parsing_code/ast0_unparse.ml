open Ast0

let rec unparse_prim = function
    IDENT(s,_) -> s
  | CHAR(s,_) -> s
  | INT(s,_) -> s
  | STR(s,_) -> s
  | SYMOP(s,_) -> s
  | TYPE(s,_) -> s
  | ARRAY(exprlist,known) ->
      Printf.sprintf "[%s%s" (unparse_expr_list exprlist)
	(if known = Ast.KNOWN then "]" else "")

and unparse_dprim = function
    DEREFOP(s,_) -> s

and unparse_expr = function
    SYMBOL(s) -> unparse_symbol s
  | DSYMBOL(s) -> unparse_dsymbol s
  | EOP(s,_) -> s
  | ASSIGN((op,_),exprlist,known) ->
      Printf.sprintf "%s %s" op (unparse_expr_list exprlist)
  | PAREN(codelist,known) ->
      Printf.sprintf "(%s%s" (unparse_code_list codelist)
	(if known = Ast.KNOWN then ")" else "")
  | STRUCT(codelist,known) ->
      Printf.sprintf "{%s%s" (unparse_code_list codelist)
	(if known = Ast.KNOWN then "}" else "")
  | CALL((nm,_),args,known) | DECLARER((nm,_),args,known) ->
      Printf.sprintf "c:%s(%s%s" nm (unparse_code_list args)
	(if known = Ast.KNOWN then ")" else "")

and unparse_symbol l = String.concat "" (List.map unparse_prim l)
and unparse_dsymbol l = String.concat "" (List.map unparse_dprim l)
and unparse_expr_list l = String.concat "|" (List.map unparse_expr l)
and unparse_code_list l = String.concat "" (List.map unparse_code l)

and unparse_code = function
    EXPR(exprlist) -> unparse_expr_list exprlist
  | SEP(s,_) -> s
