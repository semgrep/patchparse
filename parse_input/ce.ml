(* configurable things *)

let boring_ids id =
  let boring_fns id =
    (List.mem id
       ["return"; "dbg"; "err"; "dev_dbg"; "dev_err"; "dev_warn";
	 "printk"; "sprintf"; "sizeof";
	 "strcpy"; "strncpy"; "strcmp"; "strncmp"; "EXPORT_SYMBOL"]) or
    Aux.substring "debug" (String.lowercase id) in
  boring_fns id ||
  (List.mem id
     ["break"; "continue"; "for"; "if"; "while"; "switch"; "int"; "void";
       "long"; "char"; "unsigned"; "static"; "goto"; "u8"; "u16"; "u32";
       "struct";
       "uint"; "uchar"; "ulong"; "u_int"; "u_char"; "u_long"; "byte"; "NULL";
       "else"; "__FUNCTION__"])

let boring_for_expify x =
  List.mem x
    ["int";"char";"void";"unsigned";"static";"inline";"u32";"goto";"struct";
      "u8";"u16";"extern"]

let boring_for_expify2 x =
  List.mem x ["return";"break";"continue";"goto"]

let real_fn = function(*no point to codify if and while - all meaning lost*)
    Ast.SYMBOL([Ast.IDENT("if",_)])
  | Ast.SYMBOL([Ast.IDENT("while",_)])
  | Ast.SYMBOL([Ast.IDENT("switch",_)])
  | Ast.SYMBOL([Ast.IDENT("sizeof",_)])
  | Ast.SYMBOL([Ast.IDENT("for",_)])
  | Ast.SYMBOL([Ast.IDENT("memcpy",_)])
  | Ast.SYMBOL([Ast.IDENT("memset",_)])
  | Ast.SYMBOL([Ast.IDENT("kmalloc",_)])
  | Ast.SYMBOL([Ast.IDENT("kfree",_)])
  | Ast.SYMBOL([Ast.IDENT("EXPORT_SYMBOL",_)]) -> false
  | Ast.SYMBOL([Ast.IDENT(s,_)])
      when Aux.substring "debug" (String.lowercase s) -> false
  | _ -> true

(* used to let a block memory mover match up with an assignment *)
let memory_mover = function
    Ast.SYMBOL([Ast.IDENT("memcpy",_)])
  | Ast.SYMBOL([Ast.IDENT("memset",_)]) -> true
  | _ -> false

(* ---------------------------------------------------------------------- *)

type ce = (* ce stands for "change element" *)
    PRIMCE of Ast.prim * Ast.prim
  | SYMCE of Ast.symbol * Ast.symbol
  | EXPRCE of Ast.expr * Ast.expr
  | EXPRLCE of Ast.expr list * Ast.expr list
  | CODECE of Ast.code * Ast.code
  | CODELCE of Ast.code list * Ast.code list

let rebuild_ce primfn exprfn codefn = function
  | PRIMCE(prim1, prim2) ->
      let prim1 = primfn prim1 in
      let prim2 = primfn prim2 in
      PRIMCE(prim1, prim2) 
  | SYMCE(symbol1, symbol2) ->
      let symbol1 = List.map primfn symbol1 in
      let symbol2 = List.map primfn symbol2 in
      SYMCE(symbol1, symbol2)
  | EXPRCE(expr1,  expr2) ->
      let expr1 = exprfn expr1 in
      let expr2 = exprfn expr2 in
      EXPRCE(expr1, expr2)
  | EXPRLCE(expr_list1 , expr_list2) ->
      let expr_list1 = List.map exprfn expr_list1 in
      let expr_list2 = List.map exprfn expr_list2 in
      EXPRLCE(expr_list1, expr_list2)
  | CODECE(code1, code2) ->
      let code1 = codefn code1 in
      let code2 = codefn code2 in
      CODECE(code1, code2)
  | CODELCE(code_list1, code_list2) ->
      let code_list1 = List.map codefn code_list1 in
      let code_list2 = List.map codefn code_list2 in
      CODELCE(code_list1, code_list2)

let process_ce combiner mapper primfn exprfn codefn = function
  | PRIMCE(prim1, prim2) ->
      combiner (primfn prim1, primfn prim2)
  | SYMCE(symbol1,  symbol2) ->
      combiner (mapper (List.map primfn symbol1),
		mapper (List.map primfn symbol2))
  | EXPRCE(expr1,  expr2) ->
      combiner (exprfn expr1, exprfn expr2)
  | EXPRLCE(expr_list1 , expr_list2) ->
      combiner (mapper (List.map exprfn expr_list1),
		mapper (List.map exprfn expr_list2))
  | CODECE(code1, code2) ->
      combiner (codefn code1, codefn code2)
  | CODELCE(code_list1, code_list2) ->
      combiner (mapper (List.map codefn code_list1),
		mapper (List.map codefn code_list2))

(* ---------------------------------------------------------------------- *)

let rebuild_term rebuild_prim rebuild_expr rebuild_code =
  let rec do_prim prim =
    match rebuild_prim do_prim do_expr do_code prim with
      Some x -> x
    | None ->
	(match prim with
	  Ast.ARRAY(exprlist,known) ->
	    Ast.ARRAY(List.map do_expr exprlist,known)
	| Ast.PARENSYM(exprlist,known) ->
	    Ast.PARENSYM(List.map do_expr exprlist,known)
	| _ -> prim)
  and do_expr expr =
    match rebuild_expr do_prim do_expr do_code expr with
      Some x -> x
    | None ->
	(match expr with
	  Ast.SYMBOL(sym) -> Ast.SYMBOL(List.map do_prim sym)
	| Ast.ASSIGN(lhs,op,rhs,Ast.FRONTUNKNOWN) ->
	    Ast.ASSIGN(lhs,op,List.map do_expr rhs,Ast.FRONTUNKNOWN)
	| Ast.ASSIGN(lhs,op,rhs,known) ->
	    let lhs = do_expr lhs in
	    let rhs = List.map do_expr rhs in
	    Ast.ASSIGN(lhs,op,rhs,known)
	| Ast.CALL(fn,args,Ast.FRONTUNKNOWN) ->
	    Ast.CALL(fn,List.map do_code args,Ast.FRONTUNKNOWN)
	| Ast.CALL(fn,args,known) ->
	    let fn = do_expr fn in
	    let args = List.map do_code args in
	    Ast.CALL(fn,args,known)
	| Ast.PROTOTYPE(fn,x,y,z,params,known) ->
	    let fn = do_expr fn in
	    let params = List.map do_code params in
	    Ast.PROTOTYPE(fn,x,y,z,params,known)
	| Ast.STRUCT(fields,known) ->
	    Ast.STRUCT(List.map do_code fields,known)
	| _ -> expr)
  and do_code code =
    match rebuild_code do_prim do_expr do_code code with
      Some x -> x
    | None ->
	(match code with
	  Ast.EXPR(exprs) -> Ast.EXPR(List.map do_expr exprs)
	| _ -> code) in
  (do_prim,do_expr,do_code)

(* ---------------------------------------------------------------------- *)

let process_term combiner process_prim process_expr process_code =
  let rec do_prim prim =
    match process_prim (do_expr,do_code) prim with
      Some x -> x
    | None ->
	(match prim with
	  Ast.ARRAY(exprlist,known) -> combiner (List.map do_expr exprlist)
	| Ast.PARENSYM(exprlist,known) -> combiner (List.map do_expr exprlist)
	| _ -> combiner [])
  and do_expr expr =
    match process_expr (do_prim,do_code) expr with
      Some x -> x
    | None ->
	(match expr with
	  Ast.SYMBOL(sym) -> combiner (List.map do_prim sym)
	| Ast.ASSIGN(lhs,op,rhs,Ast.FRONTUNKNOWN) ->
	    combiner (List.map do_expr rhs)
	| Ast.ASSIGN(lhs,op,rhs,known) ->
	    combiner (do_expr lhs :: List.map do_expr rhs)
	| Ast.CALL(fn,args,Ast.FRONTUNKNOWN) ->
	    combiner (List.map do_code args)
	| Ast.CALL(fn,args,known) ->
	    combiner (do_expr fn :: List.map do_code args)
	| Ast.PROTOTYPE(fn,_,_,_,params,known) ->
	    combiner (do_expr fn :: List.map do_code params)
	| Ast.STRUCT(fields,known) -> combiner (List.map do_code fields)
	| _ -> combiner [])
  and do_code code =
    match process_code (do_prim,do_expr) code with
      Some x -> x
    | None ->
	(match code with
	  Ast.EXPR(exprs) -> combiner (List.map do_expr exprs)
	| _ -> combiner []) in
  (do_prim,do_expr,do_code)

(* ---------------------------------------------------------------------- *)
(* abstract line functions *)

let al_ce = rebuild_ce Ast.al_prim Ast.al_expr Ast.al_code

let have_al_ce =
  process_ce (function (x,y) -> x || y) (List.exists (function x -> x))
    Ast.have_al_prim Ast.have_al_expr Ast.have_al_code

(* ---------------------------------------------------------------------- *)
(* printing functions *)

let ce2label = function
    PRIMCE(_,_)  -> "primce"
  | SYMCE(_,_)   -> "symce"
  | EXPRCE(_,_)  -> "exprce"
  | EXPRLCE(_,_) -> "exprlce"
  | CODECE(_,_)  -> "codece"
  | CODELCE(_,_) -> "codelce"

type tex = TEX | TEXT

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
      |	TEXT -> Printf.sprintf "added\n%s\n\n" s2)
  | (s1,"") ->
      (match flag with
	TEX -> Printf.sprintf "dropped %s" (lstinline s1)
      |	TEXT -> Printf.sprintf "%s\ndropped\n\n" s1)
  | (s1,s2) ->
      (match flag with
	TEX -> 
	  Printf.sprintf "%s replaced by %s" (lstinline s1) (lstinline s2)
      |	TEXT ->
	  if (String.length s1 < !Config.page_width_threshold &&
	      String.length s2 < !Config.page_width_threshold)
	  then Printf.sprintf "%s\nreplaced by\n%s\n\n" s1 s2
	  else Printf.sprintf "replaced %s\n%s by\n%s\n\n" s1 "\n" s2)

let listify = String.concat ""

let ace2c =
  process_ce (print_replace TEXT) listify
    Ast.ast_unparse_prim Ast.ast_unparse_expr Ast.ast_unparse_code

let ce2c = function
    SYMCE(s1,s2) ->
      print_replace TEXT (Ast.unparse_symbol s1,Ast.unparse_symbol s2)
  | ce ->
      process_ce (print_replace TEXT) listify
	Ast.unparse_prim Ast.unparse_expr Ast.unparse_code ce

let ce2tex = function
    SYMCE(s1,s2) ->
      print_replace TEX (Ast.unparse_symbol s1,Ast.unparse_symbol s2)
  | ce ->
      process_ce (print_replace TEX) listify
	Ast.unparse_prim Ast.unparse_expr Ast.unparse_code ce

let ce2c_simple = function
    SYMCE(s1,s2) ->
      Printf.sprintf "%s : %s\n"(Ast.unparse_symbol s1) (Ast.unparse_symbol s2)
  | ce ->
      process_ce (function (x,y) -> Printf.sprintf "%s : %s\n" x y) listify
	Ast.unparse_prim Ast.unparse_expr Ast.unparse_code ce

(* ---------------------------------------------------------------------- *)
(* filtering functions *)

(* In some cases these are just boring, such as calls to dbg and err.  In
other cases, they do not contain enough information to determine whether
similar things are related or not, such as return, break, and continue. *)
let (boring_prim,boring_expr,boring_code,boring_ce) =
  let process_prim _ = function
      Ast.IDENT(string,_) -> Some (boring_ids string)
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
    when not (Ast.al_expr f1 = Ast.al_expr f2) ->
      (* idea: converting from one boring function to another is actually
	 interesting *)
      false
  | (CODECE(Ast.EXPR(e1),Ast.EXPR(e2))) as ce -> default ce
  | CODECE(c1,c2) -> false
  | ce -> default ce)

let boring_symbol = List.for_all boring_prim
let boring_expr_list = List.for_all boring_expr
let boring_code_list = List.for_all boring_code
      
(* ---------------------------------------------------------------------- *)
(* Abstraction *)

(* make a version of an expression with CODE replacing all function call
   arguments *)

let codify all_args l =
  List.map
    (function
	Ast.EXPR(_) -> Ast.CODE
      |	Ast.ARG(_) when all_args -> Ast.CODE
      | x -> x)
    l

let do_codify all_args =
  let rebuild_prim p e c _ = None in
  let rebuild_expr p e c = function
      Ast.CALL(fn,args,known) when real_fn fn ->
	Some (Ast.CALL(e fn,codify all_args args,known))
    | _ -> None in
  let rebuild_code p e c _ = None in
  let (codify_prim,codify_expr,codify_code) =
    rebuild_term rebuild_prim rebuild_expr rebuild_code in
  rebuild_ce codify_prim codify_expr codify_code

(* only EXPRCE used in the following two functions when called from diff.ml *)
let codify_expr_change n = do_codify false
let codify_all_args n = do_codify true
let mktail = codify_expr_change

(* ---------------------------------------------------------------------- *)
(* Abstraction, continued *)

(* Make a version of an expression with EXP replacing identifiers and some
expressions. *)

let replace n env exp =
  let rec list_assoc_al exp = 
    let al_exp1 = Ast.al_prim exp in 
    function
	[] -> raise Not_found
      | (a,b)::l ->
	  if Ast.al_prim a = al_exp1  then b else list_assoc_al exp l in
  try list_assoc_al exp !env
  with Not_found ->
    let new_name = Ast.EXP(!n,Some exp) in
    n := !n+1;
    env := (exp,new_name)::!env;
    new_name

let is_constant s = String.uppercase s = s

let expify_symbol top start n env symbol expify_prim =
 (* Find the first symbol before a . or ->, or a symbol at the end, and
  rename that.  A parensym also counts as a symbol - we replace the whole
  thing. *)
  let process_non_ids =
    List.map (function (Ast.IDENT(id,_)) as x -> x | p -> expify_prim p) in
  let rec find_dot acc = function
      [] -> (acc,[])
    | (Ast.SYMOP(op,_)::_) as rest when (op = "." || op = "->") -> (acc,rest)
    | x::rest -> find_dot (x::acc) rest in
  let (frontrev,back) = find_dot [] symbol in
  match frontrev with
    (((Ast.IDENT(id,_)) as x)::rest) ->
      let allboring =
	top &&
	List.for_all
	  (function (Ast.IDENT(id,_)) -> boring_for_expify id | _ -> false)
	  rest in
      if is_constant id || (back = [] && allboring) (* no . or -> *)
      then (List.rev frontrev)@(process_non_ids back)
      else (List.rev rest)@(replace n env x)::(process_non_ids back)
  | [(Ast.PARENSYM(_)) as x] when start && back = [] -> [expify_prim x]
  | (((Ast.PARENSYM(_)) as x)::rest) ->
      (List.rev rest)@(replace n env x)::(process_non_ids back)
  | acc -> (List.rev acc)@(process_non_ids back)

let expify do_proto expify_n ce =
  let env = ref [] in
  let rebuild_prim p e c = function
      (Ast.IDENT(id,_)) as x 
      when not (is_constant id || boring_for_expify2 id) ->
	Some (replace expify_n env x)
    | _ -> None in
  let rebuild_expr p e c = function
      Ast.SYMBOL(s) ->
	Some(Ast.SYMBOL(expify_symbol false true expify_n env s p))
    | Ast.CALL(Ast.SYMBOL(fn),args,known) ->
	Some(Ast.CALL(Ast.SYMBOL(fn),List.map c args,known))
    | Ast.PROTOTYPE((Ast.SYMBOL(fn)) as f,ty,vis,nm,args,known) ->
	if do_proto
	then
	  let newfn = (* drop modifiers *)	    
	    List.fold_left
	      (function fn ->
		function modif ->
		  List.filter
		    (function
			Ast.IDENT(x,_) -> not (x=modif)
		      | _ -> true)
		    fn)
	      fn vis in
	  let modifd = e (Ast.SYMBOL(newfn)) in
	  (* the following test is true if eg the function name is a #define
	     constant.  probably not ideal... *)
	  if Ast.al_expr modifd = Ast.al_expr f
	  then Some(Ast.PROTOTYPE(f,ty,vis,nm,List.map c args,known))
	  else
	    Some(Ast.PROTOTYPE(modifd,ty,[](*drop modifiers*),
			       "" (*drop name*),
			       List.map c args,known))
	else Some(Ast.PROTOTYPE(f,ty,vis,nm,List.map c args,known))
    | (Ast.STRUCT(code_list,known)) as x (* declaration *)
      when List.exists (function Ast.SEP(";",_) -> true | _ -> false) code_list
      -> Some x
    | _ -> None in
  let rebuild_code p e c _ = None in
  let (expify_prim,expify_expr,expify_code) =
    rebuild_term rebuild_prim rebuild_expr rebuild_code in
  match ce with
    SYMCE(os1,os2) ->
      let expify acc os1 os2 =
	let acc = List.rev acc in
	let s1 = expify_symbol true (acc=[]) expify_n env os1 expify_prim in
	let s2 = expify_symbol true (acc=[]) expify_n env os2 expify_prim in
	match (Ast.al_symbol s1 = Ast.al_symbol os1,
	       Ast.al_symbol s2 = Ast.al_symbol os2) with
	  (true,false) ->
	    let s1 =
	      expify_symbol false (acc=[]) expify_n env os1 expify_prim in
	    SYMCE(acc@s1,acc@s2)
	| (false,true) ->
	    let s2 =
	      expify_symbol false (acc=[]) expify_n env os2 expify_prim in
	    SYMCE(acc@s1,acc@s2)
	| _ -> SYMCE(acc@s1,acc@s2) in
      let rec loop acc = function
	  (x1::rest1,x2::rest2)
	  when (match x1 with Ast.SYMOP(".",_) -> true | _ -> false) or
	    (match x1 with Ast.SYMOP("->",_) -> true | _ -> false) or
	    (match x2 with Ast.SYMOP(".",_) -> true | _-> false) or
	    (match x2 with Ast.SYMOP("->",_) -> true |_ -> false) ->
	      expify [] os1 os2
	| (x1::rest1,x2::rest2) when Ast.al_prim x1 = Ast.al_prim x2 ->
	    loop (x1::acc) (rest1,rest2)
	| (x1,x2) -> expify acc x1 x2 in
      loop [] (os1,os2)
  | _ -> rebuild_ce expify_prim expify_expr expify_code ce

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
    | _ -> None in
  let process_code _ _ = None in
  let exists = List.exists (function x -> x) in
  let (prim,expr,code) =
    process_term exists process_prim process_expr process_code in
  process_ce (function (x,y) -> x || y) exists prim expr code ce
