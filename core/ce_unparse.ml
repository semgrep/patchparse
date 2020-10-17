open Ce
open Ce_visit
module Config = Globals

module U = Ast_unparse

let spunparser ct e =
  let finish before metas rawmetas invalid after =
    let depends =
      if invalid
      then
	Printf.sprintf " depends on invalid && (!select || select_rule%d)" ct
      else Printf.sprintf " depends on (!select || select_rule%d)" ct in
    let normal =
      Printf.sprintf "@rule%d%s && !prequel@\n%s%sposition __p;\n@@\n%s@__p\n%s\n\n"
	ct depends
	(String.concat "\n" metas) (if metas = [] then "" else "\n")
	before after in
    let opportunities1 =
      Printf.sprintf
	"@script:ocaml\n%s && opportunities@\np << rule%d.__p;\n@@\n"
	depends ct in
    let opportunities2 =
      Printf.sprintf
	"Printf.printf \"opportunity for rule%d in: %%s:%%s\\n\"\n  (List.hd p).file (List.hd p).current_element\n\n"
	ct in
    let prequel =
      Printf.sprintf
	"@prule%d%s && prequel@\n%s%sposition __p;\n@@\n%s@__p\n%s\n\n"
      ct depends
      (String.concat "\n" rawmetas) (if metas = [] then "" else "\n")
	before after in
    normal ^ opportunities1 ^ opportunities2 ^ prequel ^ "\n" in
  match e with
    PRIMCE(prim1, prim2) ->
      let before = U.unparse_minus U.unparse_sp_prim prim1 in
      let (rawmetas,metas,invalid,after) =
	U.unparse_plus U.unparse_sp_prim prim2 in
      finish before metas rawmetas invalid after
  | SYMCE(s1,s2) ->
      let before = U.unparse_minus U.unparse_sp_symbol s1 in
      let (rawmetas,metas,invalid,after) =
	U.unparse_plus U.unparse_sp_symbol s2 in
      finish before metas rawmetas invalid after
  | EXPRCE(expr1,  expr2) ->
      let before = U.unparse_minus U.unparse_sp_expr expr1 in
      let (rawmetas,metas,invalid,after) =
	U.unparse_plus U.unparse_sp_expr expr2 in
      finish before metas rawmetas invalid after
  | EXPRLCE(el1, el2) ->
      let before = U.unparse_minus U.unparse_sp_expr_list el1 in
      let (rawmetas,metas,invalid,after) =
	U.unparse_plus U.unparse_sp_expr_list el2 in
      finish before metas rawmetas invalid after
  | CODECE(code1, code2) ->
      let before = U.unparse_minus U.unparse_sp_code code1 in
      let (rawmetas,metas,invalid,after) =
	U.unparse_plus U.unparse_sp_code code2 in
      finish before metas rawmetas invalid after
  | CODELCE(cl1, cl2) ->
      let before = U.unparse_minus U.unparse_sp_code_list cl1 in
      let (rawmetas,metas,invalid,after) =
	U.unparse_plus U.unparse_sp_code_list cl2 in
      finish before metas rawmetas invalid after



(* ---------------------------------------------------------------------- *)
(* printing functions *)

let ce2label = function
    PRIMCE(_,_)  -> "primce"
  | SYMCE(_,_)   -> "symce"
  | EXPRCE(_,_)  -> "exprce"
  | EXPRLCE(_,_) -> "exprlce"
  | CODECE(_,_)  -> "codece"
  | CODELCE(_,_) -> "codelce"

type tex = TEX | TEXT | PARSABLE

(* get rid of latex problems in strings *)
let nstart_string n s1 s2 =
  try s1 = String.sub s2 n (String.length s1)
  with Invalid_argument _ -> false

let clean s =
  let rec loop n =
    if n = String.length s
    then []
    else
      if nstart_string n "\n" s
      then
	"\n"::
	(loop (n+(String.length "\n")))
      else
	let c = String.get s n in
	match c with
	  '{' | '}' | '&' | '#' | '_' | '%' | '\\' | '^' | '$' ->
	    (Printf.sprintf "\\%c" c)::(loop (n+1))
	| '<' | '>' -> (Printf.sprintf "\\(%c\\)" c)::(loop (n+1))
	| _ -> (Printf.sprintf "%c" c)::(loop (n+1)) in
  String.concat "" (loop 0)

let print_replace flag pair =
  let lstinline s =
    if String.contains s '\n'
    then Printf.sprintf "\\begin{verbatim}\n%s\\end{verbatim}\n" s
    else Printf.sprintf "\\lstinline¤%s¤" s in
  match pair with
    ("",s2) ->
      (match flag with
	TEX -> Printf.sprintf "added %s" (lstinline s2)
      |	TEXT -> Printf.sprintf "added\n%s\n\n" s2
      |	PARSABLE -> Printf.sprintf "added\n<!%s!>\n\n" s2)
  | (s1,"") ->
      (match flag with
	TEX -> Printf.sprintf "dropped %s" (lstinline s1)
      |	TEXT -> Printf.sprintf "%s\ndropped\n\n" s1
      |	PARSABLE -> Printf.sprintf "dropped\n<!%s!>\n\n" s1)
  | (s1,s2) ->
      (match flag with
	TEX -> 
	  Printf.sprintf "%s replaced by %s" (lstinline s1) (lstinline s2)
      |	TEXT ->
	  if (String.length s1 < !Config.page_width_threshold &&
	      String.length s2 < !Config.page_width_threshold)
	  then Printf.sprintf "%s\nreplaced by\n%s\n\n" s1 s2
	  else Printf.sprintf "replaced %s\n%s by\n%s\n\n" s1 "\n" s2
      |	PARSABLE -> 
	  Printf.sprintf "replaced\n<!%s!>\n<!%s!>\n\n" s1 s2)

let listify = String.concat ""

(* ---------------------------------------------------------------------- *)

let ace2c =
  process_ce (print_replace TEXT) listify
    Ast_unparse.ast_unparse_prim Ast_unparse.ast_unparse_expr Ast_unparse.ast_unparse_code

let unparser print_type = function
    SYMCE(s1,s2) ->
      print_replace print_type (U.unparse_symbol s1,U.unparse_symbol s2)
  | ce ->
      process_ce (print_replace print_type) listify
	U.unparse_prim U.unparse_expr U.unparse_code ce

let ce2c = unparser TEXT
let ce2tex = unparser TEX
let ce2sp = spunparser
let ce2parsable = unparser PARSABLE

let ce2c_simple = function
    SYMCE(s1,s2) ->
      Printf.sprintf "%s : %s\n"(U.unparse_symbol s1) (U.unparse_symbol s2)
  | ce ->
      process_ce (function (x,y) -> Printf.sprintf "%s : %s\n" x y) listify
	U.unparse_prim U.unparse_expr U.unparse_code ce


