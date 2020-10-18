module CE = Ce

(* Filters for various kinds of expressions *)

let contains_code l = List.mem Ast.CODE l

(* finding things in contexts *)

let rec expr_in_prim f = function
    Ast.ARRAY(exprlist,known) -> expr_in_exprlist f exprlist
  | Ast.PARENSYM(exprlist,known) -> expr_in_exprlist f exprlist
  | _ -> false

and expr_in_expr f expr =
  f expr ||
  (match expr with
     Ast.ASSIGN(lhs,op,rhs,known) ->
     expr_in_expr f lhs || expr_in_exprlist f rhs
   | Ast.CALL(fn,args,known) | Ast.DECLARER(fn,args,known) ->
     expr_in_expr f fn || expr_in_codelist f args
   | Ast.STRUCT(fields,known) -> expr_in_codelist f fields
   | _ -> false)

and expr_in_exprlist f exprlist = List.exists (expr_in_expr f) exprlist

and expr_in_code f = function
    Ast.EXPR(exprlist) -> expr_in_exprlist f exprlist
  | _ -> false

and expr_in_codelist f codelist = List.exists (expr_in_code f) codelist

(* --------------------------------------------------------------------- *)

let is_real_fn = function
    Ast.CALL(fn,_,_) | Ast.DECLARER(fn,_,_) -> Config2.real_fn fn
  | _ -> false

(* Function added *)
let addfn = function
    CE.EXPRLCE([],expr_list) ->
    expr_in_exprlist is_real_fn expr_list
  | CE.CODELCE([],code_list) ->
    expr_in_codelist is_real_fn code_list
  | _ -> false

(* Function dropped *)
let dropfn = function
    CE.EXPRLCE(expr_list,[]) ->
    expr_in_exprlist is_real_fn expr_list
  | CE.CODELCE(code_list,[]) ->
    expr_in_codelist is_real_fn code_list
  | _ -> false

(* Function change, no change of argument *)
let new_fn_same_args = function
    CE.EXPRCE(Ast.CALL(f1,a1,k1),Ast.CALL(f2,a2,k2)) ->
    not(f1=f2) && a1=a2 && k1=k2 &&
    not (contains_code a1) && not (contains_code a2)
  | CE.EXPRCE(Ast.DECLARER(f1,a1,k1),Ast.DECLARER(f2,a2,k2)) ->
    not(f1=f2) && a1=a2 && k1=k2 &&
    not (contains_code a1) && not (contains_code a2)
  | _ -> false

(* Function change, drop arguments, ignore argument order *)
let new_fn_drop_args = function
    CE.EXPRCE(Ast.CALL(f1,a1,k1),Ast.CALL(f2,a2,k2))
  | CE.EXPRCE(Ast.DECLARER(f1,a1,k1),Ast.DECLARER(f2,a2,k2)) ->
    not(f1=f2) && not(a1=a2) &&
    List.for_all (function a -> List.mem a a1) a2 &&
    List.exists (function a1 -> not(List.mem a1 a2)) a1 &&
    (if contains_code a1 then not (contains_code a2) else true) &&
    k1=k2
  | _ -> false

(* Function change, add arguments, ignore argument order *)
let new_fn_add_args = function
    CE.EXPRCE(Ast.CALL(f1,a1,k1),Ast.CALL(f2,a2,k2))
  | CE.EXPRCE(Ast.DECLARER(f1,a1,k1),Ast.DECLARER(f2,a2,k2)) ->
    not(f1=f2) && not(a1=a2) &&
    List.for_all (function a -> List.mem a a2) a1 &&
    List.exists (function a2 -> not(List.mem a2 a1)) a2 &&
    (if contains_code a2 then not (contains_code a1) else true) &&
    k1=k2
  | _ -> false

(* Function change, add and drop arguments, ignore argument order *)
let new_fn_add_and_drop_args = function
    CE.EXPRCE(Ast.CALL(f1,a1,k1),Ast.CALL(f2,a2,k2))
  | CE.EXPRCE(Ast.DECLARER(f1,a1,k1),Ast.DECLARER(f2,a2,k2)) ->
    not(f1=f2) &&
    (((contains_code a1) && (contains_code a2)) ||
     (List.exists (function a -> not (List.mem a a1)) a2 &&
      List.exists (function a -> not (List.mem a a2)) a1)) &&
    k1=k2
  | _ -> false

(* Function same, drop arguments, ignore argument order *)
let same_fn_drop_args = function
    CE.EXPRCE(Ast.CALL(f1,a1,k1),Ast.CALL(f2,a2,k2))
  | CE.EXPRCE(Ast.DECLARER(f1,a1,k1),Ast.DECLARER(f2,a2,k2)) ->
    f1=f2 && not(a1=a2) &&
    List.for_all (function a -> List.mem a a1) a2 &&
    (if contains_code a1 then not (contains_code a2) else true) &&
    k1=k2
  | _ -> false

(* Function same, add arguments, ignore argument order *)
let same_fn_add_args = function
    CE.EXPRCE(Ast.CALL(f1,a1,k1),Ast.CALL(f2,a2,k2))
  | CE.EXPRCE(Ast.DECLARER(f1,a1,k1),Ast.DECLARER(f2,a2,k2)) ->
    f1=f2 && not(a1=a2) &&
    List.for_all (function a -> List.mem a a2) a1 &&
    (if contains_code a2 then not (contains_code a1) else true) &&
    k1=k2
  | _ -> false

(* Function same, add and drop arguments, ignore argument order *)
let same_fn_add_and_drop_args = function
    CE.EXPRCE(Ast.CALL(f1,a1,k1),Ast.CALL(f2,a2,k2))
  | CE.EXPRCE(Ast.DECLARER(f1,a1,k1),Ast.DECLARER(f2,a2,k2)) ->
    f1=f2 &&
    (((contains_code a1) && (contains_code a2)) ||
     (List.exists (function a -> not (List.mem a a1)) a2 &&
      List.exists (function a -> not (List.mem a a2)) a1)) &&
    k1=k2
  | _ -> false


(* Function change, add and drop arguments, ignore argument order *)
let any_change_in_call c =
  match c with
    CE.EXPRCE(Ast.CALL(f1,a1,k1),Ast.CALL(f2,a2,k2))
  | CE.EXPRCE(Ast.DECLARER(f1,a1,k1),Ast.DECLARER(f2,a2,k2)) ->
    not(f1=f2) ||
    (((contains_code a1) && (contains_code a2)) ||
     (List.exists (function a -> not (List.mem a a1)) a2 ||
      List.exists (function a -> not (List.mem a a2)) a1))
  | _ -> false


(* --------------------------------------------------------------------- *)
(* Function declarations *)

(* same name, changed visibility *)
let prototype_changed_visibility = function
    CE.EXPRCE(Ast.PROTOTYPE(f1,t1,v1,n1,a1,k1),
              Ast.PROTOTYPE(f2,t2,v2,n2,a2,k2)) ->
    n1 = n2 && not(v1 = v2)
  | _ -> false

(* same name, changed type *)
let prototype_changed_type = function
    CE.EXPRCE(Ast.PROTOTYPE(f1,t1,v1,n1,a1,k1),
              Ast.PROTOTYPE(f2,t2,v2,n2,a2,k2)) ->
    n1 = n2 && not(t1 = t2)
  | _ -> false

let contains_exp l =
  List.exists
    (function
        Ast.EXPR([Ast.SYMBOL(s)]) ->
        List.exists (function Ast.EXP(_,_) -> true | _ -> false) s
      |	x -> false)
    l

(* same name, added parameters *)
let prototype_added_params = function
    CE.EXPRCE(Ast.PROTOTYPE(f1,t1,v1,n1,a1,k1),
              Ast.PROTOTYPE(f2,t2,v2,n2,a2,k2)) ->
    n1=n2 &&
    List.for_all (function a1 -> List.mem a1 a2) a1 &&
    List.exists (function a2 -> not(List.mem a2 a1)) a2 &&
    (if contains_exp a2 then not (contains_exp a1) else true) &&
    k1=k2
  | _ -> false

(* same name, dropped parameters *)
let prototype_dropped_params = function
    CE.EXPRCE(Ast.PROTOTYPE(f1,t1,v1,n1,a1,k1),
              Ast.PROTOTYPE(f2,t2,v2,n2,a2,k2)) ->
    n1=n2 &&
    List.for_all (function a2 -> List.mem a2 a1) a2 &&
    List.exists (function a1 -> not(List.mem a1 a2)) a1 &&
    (if contains_exp a1 then not (contains_exp a2) else true) &&
    k1=k2
  | _ -> false

(* same name, added and dropped parameters *)
let prototype_added_and_dropped_params = function
    CE.EXPRCE(Ast.PROTOTYPE(f1,t1,v1,n1,a1,k1),
              Ast.PROTOTYPE(f2,t2,v2,n2,a2,k2)) ->
    n1=n2 &&
    ((contains_exp a1 && contains_exp a2) ||
     (List.exists (function a -> not (List.mem a a1)) a2 &&
      List.exists (function a -> not (List.mem a a2)) a1)) &&
    k1=k2
  | _ -> false

(* name changed, perhaps of no interest *)
let prototype_name_changed = function
    CE.EXPRCE(Ast.PROTOTYPE(f1,t1,v1,n1,a1,k1),
              Ast.PROTOTYPE(f2,t2,v2,n2,a2,k2)) -> not (n1=n2)
  | _ -> false

(* --------------------------------------------------------------------- *)
(* Symbols *)

let rec find_first_id = function
    (Ast.IDENT(id1)::rest1,Ast.IDENT(id2)::rest2) ->
    Some(Ast.IDENT(id1),Ast.IDENT(id2),rest1,rest2)
  | (Ast.EXP(n1,l1)::rest1,Ast.EXP(n2,l2)::rest2) ->
    Some(Ast.EXP(n1,l1),Ast.EXP(n2,l2),rest1,rest2)
  | (Ast.PARENSYM(expr1,known1)::rest1,
     Ast.PARENSYM(expr2,known2)::rest2) ->
    Some(Ast.PARENSYM(expr1,known1),Ast.PARENSYM(expr2,known2),rest1,rest2)
  | (Ast.ARRAY(_,_)::rest1,Ast.ARRAY(_,_)::rest2) ->
    find_first_id (rest1,rest2)
  | (x::rest1,y::rest2) when x = y -> find_first_id (rest1,rest2)
  | _ -> None

let get_first_last prims1 prims2 =
  match find_first_id (prims1,prims2) with
    Some(start_id1, start_id2, rest1, rest2) ->
    (match find_first_id (List.rev rest1,List.rev rest2) with
       Some(end_id1, end_id2, middle1, middle2) ->
       Some(start_id1,start_id2,List.rev middle1,List.rev middle2,
            end_id1,end_id2)
     | _ -> None)
  | _ -> None

let process_symbol f = function
    CE.SYMCE(prims1,prims2) ->
    (match get_first_last prims1 prims2 with
       Some (start_id1,start_id2,middle1,middle2,end_id1,end_id2) ->
       f (start_id1,start_id2,middle1,middle2,end_id1,end_id2)
     | None -> false)
  | _ -> false

(* for all of the following, we require the same access path (ie &, * ) to
   the first identifier *)

(* Fields, same path, different field *)
let same_path_diff_field =
  process_symbol
    (function (start_id1,start_id2,middle1,middle2,end_id1,end_id2) ->
       start_id1=start_id2 && middle1=middle2 && not(end_id1=end_id2))

(* Fields, different path, same field *)
let diff_path_same_field =
  process_symbol
    (function (start_id1,start_id2,middle1,middle2,end_id1,end_id2) ->
       start_id1=start_id2 && not(middle1=middle2) && end_id1=end_id2)

(* Fields, different path and field *)
let diff_path_and_field =
  process_symbol
    (function (start_id1,start_id2,middle1,middle2,end_id1,end_id2) ->
       start_id1=start_id2 && not(middle1=middle2) && not(end_id1=end_id2))

(* --------------------------------------------------------------------- *)
(* creation of "private" fields *)

(* This could require that eg x->y becomes a call to a function that takes
   x as an argument.  For parens, we could consider with and without (more
   likely) the parentheses.  But for now, we just allow any conversion of a
   dereference to a function call, to see what happens. *)

(* no declarers here *)

let make_private = function
    CE.EXPRCE(Ast.SYMBOL(l),Ast.CALL(fn,_,_)) ->
    Config2.real_fn fn &&
    List.exists
      (function Ast.SYMOP("->",_) | Ast.SYMOP(".",_) -> true | _ -> false)
      l
  | CE.EXPRCE(Ast.CALL(Ast.SYMBOL([Ast.IDENT("if",_)]),
                       [Ast.EXPR([Ast.SYMBOL(l)])],
                       _),
              Ast.CALL(fn,_,_)) ->
    Config2.real_fn fn &&
    List.exists
      (function Ast.SYMOP("->",_) | Ast.SYMOP(".",_) -> true | _ -> false)
      l
  | CE.EXPRCE(Ast.ASSIGN(Ast.SYMBOL(l),op,rhs,k),Ast.CALL(fn,_,_)) ->
    Config2.real_fn fn &&
    List.exists
      (function Ast.SYMOP("->",_) | Ast.SYMOP(".",_) -> true | _ -> false)
      l
  | _ -> false

let make_public = function
    CE.EXPRCE(Ast.CALL(fn,_,_),Ast.SYMBOL(l)) ->
    Config2.real_fn fn &&
    List.exists
      (function Ast.SYMOP("->",_) | Ast.SYMOP(".",_) -> true | _ -> false)
      l
  | CE.EXPRCE(Ast.CALL(fn,_,_),
              Ast.CALL(Ast.SYMBOL([Ast.IDENT("if",_)]),
                       [Ast.EXPR([Ast.SYMBOL(l)])],
                       _)) ->
    Config2.real_fn fn &&
    List.exists
      (function Ast.SYMOP("->",_) | Ast.SYMOP(".",_) -> true | _ -> false)
      l
  | CE.EXPRCE(Ast.CALL(fn,_,_),Ast.ASSIGN(Ast.SYMBOL(l),op,rhs,k)) ->
    Config2.real_fn fn &&
    List.exists
      (function Ast.SYMOP("->",_) | Ast.SYMOP(".",_) -> true | _ -> false)
      l
  | _ -> false

(* --------------------------------------------------------------------- *)
(* refinement of 0 and 1 to -Exxx and 0 *)

let econst s =
  s = String.uppercase_ascii s &&
  (try String.get s 0 = 'E' with _ -> false)

(* no declarers here *)

let err_const n = function
    Ast.CALL(Ast.SYMBOL([Ast.IDENT("return",_)]),
             [Ast.EXPR([Ast.SYMBOL([Ast.INT(n1,_)])])],_) -> n1 = n
  | Ast.SYMBOL([Ast.IDENT("return",_);Ast.INT(n1,_)]) -> n1 = n
  | _ -> false

let err_id_expr = function
    Ast.CALL(Ast.SYMBOL([Ast.IDENT("return",_)]),
             [Ast.EXPR([Ast.EOP("-",_);Ast.SYMBOL([Ast.IDENT(err,_)])])],
             _) -> econst err
  | _ -> false

let err_id_expr_list = function
  | [Ast.SYMBOL([Ast.IDENT("return",_)]);
     Ast.EOP("-",_);Ast.SYMBOL([Ast.IDENT(err,_)])] -> econst err
  | _ -> false

let errorify_return = function
    CE.EXPRCE(e1,e2) when err_const "0" e1 && err_id_expr e2 -> true
  | CE.EXPRCE(e1,e2) when err_const "1" e1 && err_const "0" e2 -> true
  | CE.EXPRLCE([e1],e2) when err_const "0" e1 && err_id_expr_list e2 -> true
  | _ -> false

let unerrorify_return = function
    CE.EXPRCE(e1,e2) when err_const "0" e2 && err_id_expr e1 -> true
  | CE.EXPRCE(e1,e2) when err_const "1" e2 && err_const "0" e1 -> true
  | CE.EXPRLCE(e1,[e2]) when err_const "0" e2 && err_id_expr_list e1 -> true
  | _ -> false

(* no declarers here *)

let ifzeroone = function
    [Ast.CALL(_,_,_); Ast.EOP("==",_); Ast.SYMBOL([Ast.INT(n,_)])]
    when List.mem n ["0";"1"] -> Some (int_of_string n)
  | [Ast.CALL(_,_,_); Ast.EOP("!=",_); Ast.SYMBOL([Ast.INT(n,_)])]
    when List.mem n ["0";"1"] -> Some (if n = "0" then 1 else 0)
  | [Ast.CALL(_,_,_)] -> Some 1
  | [Ast.EOP("!",_); Ast.CALL(_,_,_)] -> Some 0
  | _ -> None

let ifnonzeroone = function
    [Ast.CALL(_,_,_); Ast.EOP("==",_); Ast.SYMBOL([Ast.INT(n,_)])]
    when List.mem n ["0";"1"] -> Some (int_of_string n)
  | [Ast.CALL(_,_,_); Ast.EOP("!=",_); Ast.SYMBOL([Ast.INT(n,_)])]
    when List.mem n ["0";"1"] -> Some (if n = "0" then 1 else 0)
  | [Ast.CALL(_,_,_); Ast.EOP(op,_); Ast.EOP("-",_);
     Ast.SYMBOL([Ast.IDENT(err)])]
    -> Some (-1)
  | [Ast.CALL(_,_,_)] -> Some 1
  | [Ast.EOP("!",_); Ast.CALL(_,_,_)] -> Some 0
  | _ -> None

let errorify_value = function
    CE.EXPRCE(Ast.CALL(Ast.SYMBOL([Ast.IDENT("if",_)]),[Ast.EXPR(arg1)],_),
              Ast.CALL(Ast.SYMBOL([Ast.IDENT("if",_)]),[Ast.EXPR(arg2)],_)) ->
    (match (ifzeroone arg1,ifnonzeroone arg2) with
       (Some 0,Some -1) -> true
     | (Some 1,Some 0) -> true
     | _ -> false)
  | CE.EXPRCE(Ast.CALL(Ast.SYMBOL([Ast.IDENT("if",_)]),[Ast.EXPR(arg1)],_),
              Ast.CALL(Ast.SYMBOL([Ast.IDENT("switch",_)]),
                       [Ast.EXPR([Ast.CALL(_,_,_)])],
                       _)) ->
    (match ifzeroone arg1 with
       Some 0 -> true
     | Some 1 -> true
     | _ -> false)
  | _ -> false

let unerrorify_value = function
    CE.EXPRCE(Ast.CALL(Ast.SYMBOL([Ast.IDENT("if",_)]),[Ast.EXPR(arg1)],_),
              Ast.CALL(Ast.SYMBOL([Ast.IDENT("if",_)]),[Ast.EXPR(arg2)],_)) ->
    (match (ifzeroone arg2,ifnonzeroone arg1) with
       (Some 0,Some -1) -> true
     | (Some 1,Some 0) -> true
     | _ -> false)
  | CE.EXPRCE(Ast.CALL(Ast.SYMBOL([Ast.IDENT("switch",_)]),
                       [Ast.EXPR([Ast.CALL(_,_,_)])],
                       _),
              Ast.CALL(Ast.SYMBOL([Ast.IDENT("if",_)]),[Ast.EXPR(arg1)],_)) ->
    (match ifzeroone arg1 with
       Some 0 -> true
     | Some 1 -> true
     | _ -> false)
  | _ -> false

(* --------------------------------------------------------------------- *)
(* Add the storage or testing of the result of a function call *)

(* no declarers here *)

let addstorage = function
    CE.EXPRCE(Ast.CALL(fn,_,_),
              Ast.CALL(Ast.SYMBOL([Ast.IDENT("if",_)]),a,k))
    when Config2.real_fn fn ->
    List.exists
      (expr_in_code (function Ast.CALL(fn1,_,_) -> fn = fn1 | _ -> false))
      a
  | CE.EXPRCE(Ast.CALL(fn,_,_),Ast.ASSIGN(lhs,op,[Ast.CALL(_,_,_)],k)) ->
    Config2.real_fn fn
  | CE.EXPRCE(Ast.CALL(fn,_,_),Ast.ASSIGN(lhs,op,rhs,k)) ->
    Config2.real_fn fn &&
    expr_in_exprlist (function Ast.CALL(fn1,_,_) -> fn = fn1 | _ -> false)
      rhs
  | _ -> false

(* Drop the storage or testing of the result of a function call *)

let dropstorage = function
    CE.EXPRCE(Ast.CALL(Ast.SYMBOL([Ast.IDENT("if",_)]),a,k),
              Ast.CALL(fn,_,_)) when Config2.real_fn fn ->
    List.exists
      (expr_in_code (function Ast.CALL(fn1,_,_) -> fn = fn1 | _ -> false))
      a
  | CE.EXPRCE(Ast.ASSIGN(lhs,op,[Ast.CALL(_,_,_)],k),Ast.CALL(fn,_,_)) ->
    Config2.real_fn fn
  | CE.EXPRCE(Ast.ASSIGN(lhs,op,rhs,k),Ast.CALL(fn,_,_)) ->
    Config2.real_fn fn &&
    expr_in_exprlist (function Ast.CALL(fn1,_,_) -> fn = fn1 | _ -> false)
      rhs
  | _ -> false

(* convert a void return to a return of a value *)
let add_return_value = function
    CE.EXPRCE(Ast.SYMBOL([Ast.IDENT("return",_)]),
              Ast.SYMBOL(Ast.IDENT("return",_)::rest)) -> true
  | CE.EXPRCE(Ast.SYMBOL([Ast.IDENT("return",_)]),
              Ast.CALL(Ast.SYMBOL([Ast.IDENT("return",_)]),_,_)) -> true
  | CE.EXPRLCE([Ast.SYMBOL([Ast.IDENT("return",_)])],
               Ast.SYMBOL([Ast.IDENT("return",_)])::rest) -> true
  | _ -> false

(* convert a return of a value to a void return *)
let drop_return_value = function
    CE.EXPRCE(Ast.SYMBOL(Ast.IDENT("return",_)::rest),
              Ast.SYMBOL([Ast.IDENT("return",_)])) -> true
  | CE.EXPRCE(Ast.CALL(Ast.SYMBOL([Ast.IDENT("return",_)]),_,_),
              Ast.SYMBOL([Ast.IDENT("return",_)])) -> true
  | CE.EXPRLCE(Ast.SYMBOL([Ast.IDENT("return",_)])::rest,
               [Ast.SYMBOL([Ast.IDENT("return",_)])]) -> true
  | _ -> false
