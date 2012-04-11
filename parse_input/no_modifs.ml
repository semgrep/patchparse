(* Drop lines that contain no modifications from the beginning and end of a
hunk *)

let contains_modif = function
    (_,Ast.Concrete fn) ->
      (match fn() with
	(_,(_,Parse_error.CTX)) -> false
      | (_,(_,Parse_error.PLUS)) | (_,(_,Parse_error.MINUS)) -> true)
  | _ -> failwith "should not occur"

let rec contains_modif_prim = function
    Ast.IDENT(string) | Ast.CHAR(string) | Ast.INT(string)
  | Ast.SYMOP(string) -> contains_modif string
  | Ast.STR (string) -> contains_modif string
  | Ast.ARRAY(exprlist,known) ->
      contains_modif_expr_list exprlist
  | Ast.PARENSYM(exprlist,known) ->
      contains_modif_expr_list exprlist
  | Ast.EXP(n,_) -> failwith "should not appear yet"

and contains_modif_symbol primlist = List.exists contains_modif_prim primlist

and contains_modif_expr = function
    Ast.SYMBOL(symbol) -> contains_modif_symbol symbol
  | Ast.EOP(string) -> contains_modif string
  | Ast.ASSIGN(symbol,(op),exprlist,known) ->
      (contains_modif_expr symbol) or (contains_modif op) or
      (contains_modif_expr_list exprlist)
  | Ast.CALL(symbol,codelist,known) ->
      (contains_modif_expr symbol) or
      (contains_modif_code_list codelist)
  | Ast.PROTOTYPE(symbol,ty,attrs,nm,codelist,known) ->
      (contains_modif_expr symbol) or
      (contains_modif_symbol ty) or
      (contains_modif_code_list codelist)
  | Ast.STRUCT(codelist,known) ->
      contains_modif_code_list codelist

and contains_modif_code = function
    Ast.EXPR(exprlist) -> contains_modif_expr_list exprlist
  | Ast.SEP(string) -> contains_modif string
  | Ast.ARG(n) -> failwith "should not appear yet"
  | Ast.CODE   -> failwith "should not appear yet"

and contains_modif_code_list l = List.exists contains_modif_code l
and contains_modif_expr_list l = List.exists contains_modif_expr l
and contains_modif_symbol_list l = List.exists contains_modif_symbol l

let drop_outer l =
  let rec drop_inner = function
      [] -> []
    | x::xs ->
	if contains_modif_code x
	then x::xs
	else drop_inner xs in
  let rec drop_first dir = function
      [] -> []
    | x::xs ->
	(match drop_inner (dir x) with
	  [] -> drop_first dir xs
	| rest -> (dir rest) :: xs) in
  let id x = x in
  let l = drop_first id l in
  let l = List.rev(drop_first List.rev (List.rev l)) in
  l
