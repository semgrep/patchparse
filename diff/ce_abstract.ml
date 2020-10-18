open Ce
open Ce_visit
open Ast_visit

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
      Ast.CALL(fn,args,known) when Config2.real_fn fn ->
      Some (Ast.CALL(e fn,codify all_args args,known))
    | Ast.DECLARER(fn,args,known) when Config2.real_fn fn ->
      Some (Ast.DECLARER(e fn,codify all_args args,known))
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
    let al_exp1 = Ast_al.al_prim exp in 
    function
      [] -> raise Not_found
    | (a,b)::l ->
      if Ast_al.al_prim a = al_exp1  then b else list_assoc_al exp l in
  try list_assoc_al exp !env
  with Not_found ->
    let new_name = Ast.EXP(!n,Some exp) in
    n := !n+1;
    env := (exp,new_name)::!env;
    new_name

let is_constant s = String.uppercase_ascii s = s

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
        (function (Ast.IDENT(id,_)) -> Config2.boring_for_expify id | _ -> false)
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
      when not (is_constant id || Config2.boring_for_expify2 id) ->
      Some (replace expify_n env x)
    | _ -> None in
  let rebuild_expr p e c = function
      Ast.SYMBOL(s) ->
      Some(Ast.SYMBOL(expify_symbol false true expify_n env s p))
    | Ast.CALL(Ast.SYMBOL(fn),args,known) ->
      Some(Ast.CALL(Ast.SYMBOL(fn),List.map c args,known))
    | Ast.DECLARER(Ast.SYMBOL(fn),args,known) ->
      Some(Ast.DECLARER(Ast.SYMBOL(fn),List.map c args,known))
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
        if Ast_al.al_expr modifd = Ast_al.al_expr f
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
      match (Ast_al.al_symbol s1 = Ast_al.al_symbol os1,
             Ast_al.al_symbol s2 = Ast_al.al_symbol os2) with
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
        when (match x1 with Ast.SYMOP(".",_) -> true | _ -> false) ||
             (match x1 with Ast.SYMOP("->",_) -> true | _ -> false) ||
             (match x2 with Ast.SYMOP(".",_) -> true | _-> false) ||
             (match x2 with Ast.SYMOP("->",_) -> true |_ -> false) ->
        expify [] os1 os2
      | (x1::rest1,x2::rest2) when Ast_al.al_prim x1 = Ast_al.al_prim x2 ->
        loop (x1::acc) (rest1,rest2)
      | (x1,x2) -> expify acc x1 x2 in
    loop [] (os1,os2)
  | _ -> rebuild_ce expify_prim expify_expr expify_code ce

