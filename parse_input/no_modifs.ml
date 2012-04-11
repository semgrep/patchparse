(* Drop lines that contain no modifications from the beginning and end of a
hunk *)

let contains_modif = function
    (_,(_,Parse_error.CTX)) -> false
  | (_,(_,Parse_error.PLUS)) | (_,(_,Parse_error.MINUS)) -> true

let rec contains_modif_prim = function
    IDENT(string) | CHAR(string) | INT(string) | STR (string)
  | SYMOP(string) -> contains_modif string
  | ARRAY(exprlist,known) ->
      (contains_modif_expr_list exprlist) or
      (contains_modif_known known)
  | PARENSYM(exprlist,known) ->
      (contains_modif_expr_list exprlist) or
      (contains_modif_known known)
  | EXP(n,_) -> failwith "should not appear yet"

and contains_modif_symbol primlist = List.exists contains_modif_prim primlist

and contains_modif_expr = function
    SYMBOL(symbol) -> contains_modif_symbol symbol
  | EOP(string) -> contains_modif string
  | ASSIGN(symbol,(op),exprlist,known) ->
      (contains_modif_expr symbol) or (contains_modif op) or
      (contains_modif_expr_list exprlist)
  | CALL(symbol,codelist,known) ->
      (contains_modif_expr symbol) or
      (contains_modif_code_list codelist)
  | PROTOTYPE(symbol,ty,attrs,nm,codelist,known) ->
      (contains_modif_expr symbol) or
      (contains_modif_symbol ty) or
      (contains_modif_code_list codelist) or
      (contains_modif_known known)
  | STRUCT(codelist,known) ->
      contains_modif_code_list codelist

and contains_modif_code = function
    EXPR(exprlist) -> contains_modif_expr_list exprlist
  | SEP(string) -> contains_modif string
  | ARG(n) -> failwith "should not appear yet"
  | CODE   -> failwith "should not appear yet"

and contains_modif_code_list l = List.exists contains_modif_code l
and contains_modif_expr_list l = List.exists contains_modif_expr l
and contains_modif_symbol_list l = List.exists contains_modif_symbol l

let drop_outer l =
  let rec drop_first = function
      [] -> failwith "no changes???"
    | x::xs ->
	if contains_modif_code_list x
	then x::xs
	else drop_first xs in
  let l = drop_first l in
  let l = List.rev(drop_first(List.rev l)) in
  l
