open Ast


(* ---------------------------- unparser --------------------------- *)
(* Produces an AST *)

let listify l = Printf.sprintf "[%s]" (String.concat ";" l)

let ast_unparse_known = function
    KNOWN -> "known"
  | ENDUNKNOWN -> "end_unknown"
  | FRONTUNKNOWN -> "front_unknown"
  | BOTHUNKNOWN -> "both_unknown"

let rec ast_unparse_prim = function
    IDENT(string) -> Printf.sprintf "IDENT(%s)" (eext string)
  | CHAR(string) -> Printf.sprintf "CHAR(%s)" (eext string)
  | INT(string) -> Printf.sprintf "INT(%s)" (eext string)
  | STR (_) (*string*) -> Printf.sprintf "STR"
  | SYMOP(string) -> Printf.sprintf "SYMOP(%s)" (eext string)
  | ARRAY(exprlist,known) ->
      Printf.sprintf "ARRAY(%s,%s)"
	(ast_unparse_expr_list exprlist)
	(ast_unparse_known known)
  | PARENSYM(exprlist,known) ->
      Printf.sprintf "PARENSYM(%s,%s)"
	(ast_unparse_expr_list exprlist)
	(ast_unparse_known known)
  | EXP(n,_) -> Printf.sprintf "EXP(%d)" n

and ast_unparse_symbol primlist = listify (List.map ast_unparse_prim primlist)

and ast_unparse_expr = function
    SYMBOL(symbol) -> Printf.sprintf "SYMBOL(%s)" (ast_unparse_symbol symbol)
  | EOP(string) -> Printf.sprintf "EOP(%s)" (eext string)
  | ASSIGN(symbol,(op),exprlist,known) ->
      Printf.sprintf "ASSIGN(%s,%s,%s,%s)" (ast_unparse_expr symbol) (eext op)
	(ast_unparse_expr_list exprlist)
	(ast_unparse_known known)
  | CALL(symbol,codelist,known) ->
      Printf.sprintf "CALL(%s,%s,%s)" (ast_unparse_expr symbol)
	(ast_unparse_code_list codelist)
	(ast_unparse_known known)
  | DECLARER(symbol,codelist,known) ->
      Printf.sprintf "DECLARER(%s,%s,%s)" (ast_unparse_expr symbol)
	(ast_unparse_code_list codelist)
	(ast_unparse_known known)
  | PROTOTYPE(symbol,ty,attrs,nm,codelist,known) ->
      Printf.sprintf "PROTOTYPE(%s,(%s,%s,%s),%s,%s)" (ast_unparse_expr symbol)
	(ast_unparse_symbol ty) (String.concat " " attrs) nm
	(ast_unparse_code_list codelist)
	(ast_unparse_known known)
  | STRUCT(codelist,known) ->
      Printf.sprintf "STRUCT(%s,%s)"
	(ast_unparse_code_list codelist)
	(ast_unparse_known known)

and ast_unparse_code = function
    EXPR(exprlist) ->
      Printf.sprintf "EXPR(%s)" (ast_unparse_expr_list exprlist)
  | SEP(string) -> Printf.sprintf "SEP(%s)" (eext string)
  | ARG(n) -> Printf.sprintf "ARG(%d)" n
  | CODE -> Printf.sprintf "CODE"

and ast_unparse_code_list l = listify (List.map ast_unparse_code l)
and ast_unparse_expr_list l = listify (List.map ast_unparse_expr l)
and ast_unparse_symbol_list l = listify (List.map ast_unparse_symbol l)

(* ---------------------------- unparser --------------------------- *)
(* Produces a string *)

let get_space = function
    IDENT(string) -> " "
  | CHAR(string) -> " "
  | INT(string) -> " "
  | STR (_) (*string*) -> " "
  | SYMOP("*",_) -> " "
  | SYMOP(string) -> ""
  | ARRAY(exprlist,known) -> ""
  | PARENSYM(exprlist,known) -> ""
  | EXP(_,_) -> " "

(*let get_space _ = ""*)

let rec local_unparse_prim after = function
    IDENT(string,_) -> string^after
  | CHAR(string,_) -> "'"^string^"'"^after
  | INT(string,_) -> string^after
  | STR (_) (*string*) -> "\"STRING\""^after
  | SYMOP(string,_) -> string
  | ARRAY(exprlist,known) ->
      Printf.sprintf "[%s%s"
	(unparse_expr_list exprlist) (if known = KNOWN then "]" else "")
  | PARENSYM(exprlist,known) ->
      Printf.sprintf "(%s%s"
	(unparse_expr_list exprlist) (if known = KNOWN then ")" else "")
  | EXP(n,_) -> Printf.sprintf "EXP%d" n

and unparse_prim prim = local_unparse_prim "" prim

and unparse_symbol primlist =
  let rec loop = function
      [] -> []
    | [x] -> [local_unparse_prim "" x]
    | x::((y::_) as rest) ->
	(local_unparse_prim (get_space y) x)::(loop rest) in
  String.concat "" (loop primlist)

and unparse_expr = function
    SYMBOL(symbol) -> unparse_symbol symbol
  | EOP(string,_) -> string
  | ASSIGN(symbol,(op,_),exprlist,_) ->
      Printf.sprintf "%s %s %s" (unparse_expr symbol) op
	(unparse_expr_list exprlist)
  | CALL(symbol,codelist,known) | DECLARER(symbol,codelist,known) ->
      Printf.sprintf "%s(%s%s" (unparse_expr symbol)
	 (unparse_code_list codelist)
	(if known = KNOWN || known = FRONTUNKNOWN then ")" else "")
  | PROTOTYPE(symbol,_,_,_,codelist,known) ->
      Printf.sprintf "%s(%s%s" (unparse_expr symbol)
	 (unparse_code_list codelist)
	(if known = KNOWN || known = FRONTUNKNOWN then ")" else "")
  | STRUCT(codelist,known) ->
      Printf.sprintf "{%s%s" (unparse_code_list codelist)
	(if known = KNOWN then "}" else "")

and unparse_code = function
    EXPR(exprlist) ->
      Printf.sprintf "%s" (String.concat " " (List.map unparse_expr exprlist))
  | SEP(";",_) -> Printf.sprintf ";\n"
  | SEP(string,_) -> Printf.sprintf "%s " string
  | ARG(n) -> Printf.sprintf "ARG%d" n
  | CODE -> Printf.sprintf "CODE"

and unparse_symbol_list l = String.concat "" (List.map unparse_symbol l)
and unparse_expr_list l = String.concat "" (List.map unparse_expr l)
and unparse_code_list l = String.concat "" (List.map unparse_code l)

let unparse l = String.concat "" (List.map unparse_code l)

(* ----------------------------- unparse sp ----------------------------- *)

let code_counter = ref 0
let raw_metavariables = ref ([] : string list)
let metavariables = ref ([] : string list)
let metanames = ref ([] : string list)
let invalid = ref false

let new_meta s =
  let res = Printf.sprintf "%s%d" s !code_counter in
  code_counter := !code_counter + 1;
  res

let do_add_meta rawty ty name =
  metanames := name :: !metanames;
  raw_metavariables := (* real metavars for invalids *)
    Printf.sprintf "%s %s;" rawty name :: !raw_metavariables;
  metavariables := Printf.sprintf "%s %s;" ty name :: !metavariables

let add_meta ty name = do_add_meta ty ty name
let invalid_add_meta rawty ty name = do_add_meta rawty ty name

let rec local_unparse_sp_prim minus after = function
    IDENT(string,_) -> string^after
  | CHAR(string,_) -> "'"^string^"'"^after
  | INT(string,_) -> string^after
  | STR (_) (*string*) ->
      let str = new_meta "string" in
      (if not minus then invalid := true);
      add_meta "constant char []" str;
      str^after
  | SYMOP(string,_) -> string
  | ARRAY(exprlist,known) ->
      Printf.sprintf "[%s%s"
	(unparse_sp_expr_list minus exprlist)
	(if known = KNOWN then "]" else "")
  | PARENSYM(exprlist,known) ->
      Printf.sprintf "(%s%s"
	(unparse_sp_expr_list minus exprlist)
	(if known = KNOWN then ")" else "")
  | EXP(n,_) ->
      let exp = Printf.sprintf "EXP%d" n in
      (if not (List.mem exp !metanames)
      then
	begin
	  (if not minus then invalid := true);
	  invalid_add_meta "expression"
	    (if minus then "expression" else "symbol")
	    exp
	end);
      exp

and unparse_sp_prim minus = local_unparse_sp_prim minus ""

and unparse_sp_symbol minus primlist =
  let rec loop = function
      [] -> []
    | [x] -> [local_unparse_sp_prim minus "" x]
    | x::((y::_) as rest) ->
	(local_unparse_sp_prim minus (get_space y) x)::(loop rest) in
  String.concat "" (loop primlist)

and unparse_sp_expr minus = function
    SYMBOL(symbol) -> unparse_sp_symbol minus symbol
  | EOP(string,_) -> string
  | ASSIGN(symbol,(op,_),exprlist,_) ->
      Printf.sprintf "%s %s %s" (unparse_sp_expr minus symbol) op
	(unparse_sp_expr_list minus exprlist)
  | CALL(symbol,codelist,known) | DECLARER(symbol,codelist,known) ->
      let fn = unparse_sp_expr minus symbol in
      let res fn =
	Printf.sprintf "%s(%s%s" fn
	  (unparse_sp_code_list minus codelist)
	  (if known = KNOWN || known = FRONTUNKNOWN then ")" else "...)") in
      if fn = "if"
      then
	begin
	  let s1 = new_meta "S" in
	  let s2 = new_meta "S" in
	  add_meta "statement" s1;
	  add_meta "statement" s2;
	  Printf.sprintf "%s\n%s else %s" (res fn) s1 s2
	end
      else res fn
  | PROTOTYPE(symbol,_,_,_,codelist,known) ->
      Printf.sprintf "%s(%s%s" (unparse_sp_expr minus symbol)
	 (unparse_sp_code_list minus codelist)
	(if known = KNOWN || known = FRONTUNKNOWN then ")" else "...)")
  | STRUCT(codelist,known) ->
      Printf.sprintf "{%s%s" (unparse_sp_code_list minus codelist)
	(if known = KNOWN then "}" else "...}")

and unparse_sp_code minus = function
    EXPR(exprlist) ->
      Printf.sprintf "%s"
	(String.concat " " (List.map (unparse_sp_expr minus) exprlist))
  | SEP(";",_) -> ";\n"
  | SEP(string,_) -> Printf.sprintf "%s " string
  | ARG(n) ->
      let exp = Printf.sprintf "ARG%d" n in
      (if minus then add_meta "expression" exp);
      exp
  | CODE ->
      let exp = new_meta "CODE" in
      invalid_add_meta "expression"
	(if minus then "expression" else "symbol")
	exp;
      (if not minus then invalid := true);
      exp

and unparse_sp_symbol_list minus l =
  String.concat "" (List.map (unparse_sp_symbol minus) l)
and unparse_sp_expr_list minus l =
  String.concat "" (List.map (unparse_sp_expr minus) l)
and unparse_sp_code_list minus l =
  String.concat "" (List.map (unparse_sp_code minus) l)

let unparse_minus fn l =
  code_counter := 0;
  metavariables := [];
  raw_metavariables := [];
  metanames := [];
  invalid := false;
  let res = fn true l in
  if res = ""
  then res
  else
    let res = String.concat "\n- " (Str.split (Str.regexp_string "\n") res) in
    Printf.sprintf "- %s" res
let unparse_plus fn l =
  let res = fn false l in
  if res = ""
  then (List.rev !raw_metavariables,List.rev !metavariables,false,res)
  else
    let res = String.concat "\n+ " (Str.split (Str.regexp_string "\n") res) in
    (List.rev !raw_metavariables,List.rev !metavariables,
     !invalid,Printf.sprintf "+ %s" res)





