open Ast0
open Ast0_unparse
module Config = Globals

let __unknown_line = (-1,Patch.CTX)


(* ----------------------------------------------------------------- *)
(* convert ast0 to ast *)
(* This entails identifying function calls and assignments.  This is done
   by considering expression lists, ie the code between two separators.

   An assignment can only appear at the end of an expression list, by the
   definition of the parser.  Everything between the rightmost top-level SYMOP
   becomes the left-hand side, or if there is none everything up to the
   beginning of the expression list.  If the beginning of what is collected is
   a DEREFOP, we have to consider whether it is a dereference or a
   multiplication.  If we search back and find a SYMOP before finding anything
   other than another DEREFOP, then it is a dereference and is part of the
   symbol.  On the other hand, if we find something else, then the leftmost
   DEREFOP is actually a multiplication.

   A function call can appear anywhere in an expression list.  Once we find a
   PAREN, we search for the rightmost PAREN before encountering anything else.
   This PAREN is a candidate for a function call.  If its body contains more
   than one thing, it is definitely a function call, and we identify the
   function in the same way as we just identified the left-hand side of an
   assignment.  If its body contains only one thing, then we look at the
   previous thing.  If it is also a PAREN, and this contains only types, then
   it is not a function call, and we move on to the right to continue
   searching for function calls.  In any other case, as long as there is
   something to the left that might possibly be a function name, then we
   assume it is a function call and identify the function name as before.
   Note that a sequence of only DEREFOP cannot be a function name, nor can a
   sequence that contains only characters/integers/strings/types. *)

(* can be all types or types followed by dereferences, ie a symbol and a
   dsymbol or just a symbol.  Just one dereference, ie "( * )" is also allowed. *)
let is_cast exprlist =
  let rec loop = function
      [] -> true
    | IDENT(s,_)::rest -> false
    | CHAR(s,_)::rest -> false
    | INT(s,_)::rest -> false
    | STR(s,_)::rest -> false
    | SYMOP(s,_)::rest -> false
    | [TYPE("struct",_);IDENT(s,_)] -> true
    | TYPE(s,_)::rest -> loop rest
    | ARRAY(exprlist,known)::rest -> false in
  match exprlist with
    [SYMBOL(only_types)] -> loop only_types
  | [SYMBOL(only_types);DSYMBOL(only_stars)] ->
    loop only_types &&
    List.for_all (function DEREFOP(s,_) -> s = "*") only_stars
  | [DSYMBOL([DEREFOP("*",_)])] -> true
  | _ -> false

let make_front_unknown = function
    Ast.KNOWN -> Ast.FRONTUNKNOWN
  | Ast.ENDUNKNOWN -> Ast.BOTHUNKNOWN
  | _ -> failwith "not possible"

(* Drop substrings that correspond to the file name.  Drop any leading or
   trailing _'s as well *)
let mkident s =
  let filename = !Config.filename in
  let prefilename = List.hd (Str.split (Str.regexp "_") filename) in
  let process fl s =
    match Str.bounded_split_delim (Str.regexp (fl^"_")) s 2 with
      [bef;aft] -> (*found a match*) bef^aft
    | _ -> (*found no match*)
      (try
         let ender =
           String.sub s (String.length s - String.length fl)
             (String.length fl) in
         if ender = fl
         then String.sub s 0 (String.length s - String.length fl)
         else s
       with _-> s) in
  let e1 = process filename s in
  if e1 = s
  then process prefilename s
  else e1

(* some heuristics to try to distinguish a function declaration from a
   function call *)
let function_declaration fn args =
  let rec all_stars = function
      [] -> false
    | Ast.SYMOP("*",_)::rest -> all_stars rest
    | [Ast.IDENT(_)] -> true
    | _ -> false in
  let rec all_ids = function
      [] -> true
    | Ast.SYMOP("*",_)::rest -> all_stars rest
    | Ast.IDENT(_)::rest -> all_ids rest
    | _ -> false in
  all_ids fn &&
  (  (*old: (args = [Ast.EXPR([Ast.SYMBOL([Ast.IDENT("void")])])]) *)
    (match args with
       [Ast.EXPR([Ast.SYMBOL([Ast.IDENT("void",_)])])] -> true
     | _ -> false) ||
    (all_ids fn &&
     (List.exists
        (function Ast.EXPR([Ast.SYMBOL(s)]) -> List.length s > 1 | _ -> false)
        args) &&
     (List.for_all
        (function
            Ast.EXPR([Ast.SYMBOL(s)]) -> all_ids s 
          | Ast.SEP(",",_) -> true
          | _ -> false)
        args)))

let parse_function_name l =
  match List.rev l with
    Ast.IDENT(name,_)::rest ->
    let rest = List.rev rest in
    let rec find_front acc = function
        [] -> (acc,[])
      | Ast.IDENT("static",_)::rest -> find_front ("static"::acc) rest
      | Ast.IDENT("inline",_)::rest -> find_front ("inline"::acc) rest
      | (Ast.IDENT(s,_)::rest) as all ->
        (try
           if String.sub s 0 2 = "__"
           then find_front (s::acc) rest
           else (acc,all)
         with Invalid_argument _ -> (acc,all))
      | all -> (acc,all) in
    let (acc,rest) = find_front [] rest in
    let (acc,rest) = find_front acc (List.rev rest) in
    (rest,List.sort compare acc,name)
  | _ -> ([],[],"")

let rec convert_prim = function
    IDENT(s,_line) -> Ast.IDENT(Ast.bext (mkident s, _line))
  | CHAR(s,_line) -> Ast.CHAR(Ast.bext (s, _line))
  | INT(s,_line) -> Ast.INT(Ast.bext (s, _line))
  | STR(s,_line) -> Ast.STR (Ast.bext ((),_line)) (*s*)
  | SYMOP(s,_line) -> Ast.SYMOP(Ast.bext (s, _line))
  | TYPE(s,_line) -> Ast.IDENT(Ast.bext (mkident s, _line))
  | ARRAY(exprlist,known) ->
    Ast.ARRAY(convert_exprlist false false exprlist,known)

and convert_dprim = function
    DEREFOP(s,_line) -> Ast.SYMOP(Ast.bext (s,_line))

and convert_symbol primlist = List.map convert_prim primlist
and convert_dsymbol dprimlist = List.map convert_dprim dprimlist

and convert_expr = function
    DSYMBOL(dsym) -> Ast.SYMBOL(convert_dsymbol dsym)
  | EOP(s,_line) -> Ast.EOP(Ast.bext (s,_line))
  | STRUCT(codelist,known) -> Ast.STRUCT(convert_codelist false codelist,known)
  | CALL((nm,_line),args,known) ->
    Ast.CALL(Ast.SYMBOL([Ast.IDENT(Ast.bext (mkident nm, _line))]),
             convert_codelist false args, known)
  | DECLARER((nm,_line),args,known) ->
    Ast.DECLARER(Ast.SYMBOL([Ast.IDENT(Ast.bext (mkident nm, _line))]),
                 convert_codelist false args, known)
  | e ->
    failwith
      (Printf.sprintf "convert_expr: not possible: %s" (unparse_expr e))

and convert_code start = function
    EXPR(exprlist) -> Ast.EXPR(convert_exprlist true start exprlist)
  | SEP(s,_line) -> Ast.SEP(Ast.bext (s,_line))

and convert_codelist start codelist =
  match codelist with
    first::rest ->
    (convert_code (start&&true) first) :: (convert_codelist false rest)
  | [] -> []

and top_convert_codelist start codelist =
  match codelist with
    EXPR([SYMBOL(fn);PAREN(args,known)])::SEP("{",_line2)::rest ->
    let newfn = convert_symbol fn in
    let (ty,annotations,name) = parse_function_name newfn in
    (match top_convert_codelist start rest with
       hd::tl ->
       (Ast.EXPR([Ast.PROTOTYPE(Ast.SYMBOL(newfn),ty,annotations,name,
                                convert_codelist false args,known)])::
        Ast.SEP(Ast.bext ("{",_line2))::hd)::tl
     |	_ -> failwith "not possible")
  | first::rest ->
    let rest = top_convert_codelist false rest in
    (match rest with
       ((Ast.EXPR([Ast.PROTOTYPE(_,_,_,_,_,_)])::_)::_) ->
       (* could try to do something with struct declarations *)
       [convert_code (start&&true) first]::rest
     |	hd::others -> ((convert_code (start&&true) first)::hd)::others
     |	[] -> failwith "not possible")
  | [] -> [[]]

(* A (ast) symbol is at most a dsymbol followed by a sequence of things in
   parentheses, each containing only one item and all but the last being casts
   (the last can be a case too), followed by a symbol.  Any, but not all, of
   these can be omitted.  Something other than dsymbol must be present.  l is
   reversed, so these things appear in the opposite order. For an assignment,
   there can also be a : at the end. *)
and find_symbol l in_assignment =
  (* how to get the pieces *)
  let rec get_type = function
      SYMBOL(s)::rest ->
      let rec ident_prefix = function
          [] -> ([],[])
        | IDENT(i,_line)::rest ->
          let (pref,tail) = ident_prefix rest in
          (IDENT(i,_line)::pref,tail)
        | TYPE(i,_line)::rest ->
          let (pref,tail) = ident_prefix rest in
          (TYPE(i,_line)::pref,tail)
        | rest -> ([],rest) in
      let (pref,tail) = ident_prefix s in
      (convert_symbol pref,
       if tail = [] then rest else SYMBOL(tail)::rest)
    | rest -> ([],rest) in
  let get_deref = function
      DSYMBOL(s)::rest -> (* should we check for where & occurs? *)
      ((convert_dsymbol s),rest)
    | rest -> ([],rest) in
  let rec get_casts = function
      PAREN([EXPR(arg)],known)::rest when is_cast arg ->
      let (casts,rest) = get_casts rest in
      ((mkparen arg known)::casts,rest)
    | rest -> ([],rest) in
  let get_parens = function
      PAREN([EXPR(arg)],known)::rest -> (Some (mkparen arg known),rest)
    | rest -> (None,rest) in
  let get_symbol = function
      SYMBOL(s)::rest -> (convert_symbol s,rest)
    | rest -> ([],rest) in
  let get_colon = function
      EOP(":",_line)::rest when in_assignment ->
      ([Ast.SYMOP(Ast.bext (":",_line))],rest)
    | rest -> ([],rest) in
  (* getting the pieces *)
  let (colon_part,rest) = get_colon l in
  let (symbol_part,rest) = get_symbol rest in
  let (parens,rest) =
    match get_parens rest with
      (Some exprlist,rest) ->
      let (casts,rest) = get_casts rest in
      let casts = List.rev (exprlist :: casts) in
      (casts,rest)
    | (None,rest) -> ([],rest) in
  let (deref_part,rest) = get_deref rest in
  let (decl_part,rest) =
    if List.for_all (function Ast.SYMOP("*",_) -> true | _ -> false) deref_part
    then get_type rest
    else ([],rest) in
  let newsym = decl_part@deref_part@parens@symbol_part in
  if Ast_al.al_symbol newsym = Ast_al.al_symbol deref_part (* PAD *)
  then (None,rest)
  else
    match newsym with
      [] -> (None,rest)
    | sym -> (Some sym,rest)


and mkparen exprlist known =
  Ast.PARENSYM(convert_exprlist true false exprlist,known)

and convert_exprlist no_fn_name_allowed no_assign_lhs_allowed exprlist =
  let backwards = List.rev exprlist in
  let rec loop = function
      [] -> []
    (* symbols: merge with preceeding non-call ()'s and derefs *)
    | (SYMBOL(sym)::_) as rest ->
      (match find_symbol rest false with
         (None,rest) -> failwith "symbol: can't happen"
       | (Some sym,rest) -> Ast.SYMBOL(sym)::loop rest)
    (* assignments: find the left-hand side *)
    | (ASSIGN(op,exprlist,known))::
      ((CALL _ | DECLARER _ ) as call)::rest ->
      Ast.ASSIGN(convert_expr call,Ast.bext op,
                 convert_exprlist true false exprlist,known) ::
      loop rest
    | (ASSIGN(op,exprlist,known) as e)::rest ->(* must be in the first position *)
      let (lhs,rest) = find_symbol rest true in
      (match lhs with
         None ->
         if rest = [] && no_assign_lhs_allowed
         then
           let known = make_front_unknown known in 
           Ast.ASSIGN(Ast.SYMBOL([Ast.IDENT(Ast.bext ("",__unknown_line))]),
                      Ast.bext op,
                      convert_exprlist true false exprlist,known) ::
           loop rest
         else
           (match rest with
              e1::e2::_ ->
              (match (e1,e2) with
                 (PAREN _,PAREN([EXPR[DSYMBOL[DEREFOP _];SYMBOL[IDENT(x,_)]]],Ast.KNOWN)) ->
                 (* assumed to be a function pointer type init, just keep the name *)
                 Ast.ASSIGN(Ast.SYMBOL([Ast.IDENT(Ast.bext (x,__unknown_line))]),
                            Ast.bext op,
                            convert_exprlist true false exprlist,known) ::
                 loop rest
               | (PAREN _,PAREN([EXPR[DSYMBOL[DEREFOP _];SYMBOL[IDENT(x,_);ARRAY(_,Ast.KNOWN)]]],Ast.KNOWN)) ->
                 (* assumed to be a function pointer type init, just keep the name *)
                 Ast.ASSIGN(Ast.SYMBOL([Ast.IDENT(Ast.bext (x,__unknown_line))]),
                            Ast.bext op,
                            convert_exprlist true false exprlist,known) ::
                 loop rest
               | _ ->
                 failwith
                   (Printf.sprintf
                      "missing assignment left hand side: e1: %s === e2: %s === e: %s"
                      (unparse_expr e1) (unparse_expr e2) (unparse_expr e)))
            | e1::_ ->
              failwith
                (Printf.sprintf
                   "missing assignment left hand side: %s %s"
                   (unparse_expr e1) (unparse_expr e))
            | [] ->
              failwith
                (Printf.sprintf
                   "no available assignment left hand side: %s"
                   (unparse_expr e)))
       | Some lhs ->
         Ast.ASSIGN(Ast.SYMBOL(lhs),Ast.bext op,
                    convert_exprlist true false exprlist,known) ::
         loop rest)
    (* paren: determine whether it is a function call *)
    | [PAREN([EXPR(arg)],known)] ->
      [Ast.SYMBOL([mkparen arg known])]
    | PAREN([EXPR(arg)],known)::EOP(s,_line)::rest ->
      Ast.SYMBOL([Ast.PARENSYM(convert_exprlist true false arg,known)])::
      Ast.EOP(Ast.bext (s,_line))::loop rest
    | PAREN([EXPR(arg)],known)::STRUCT(codelist,known1)::rest ->
      Ast.SYMBOL([mkparen arg known])::
      Ast.STRUCT(convert_codelist false codelist,known1)::loop rest
    | PAREN([EXPR(arg)],known)::PAREN([EXPR(arg1)],known1)::
      DSYMBOL(derefs)::rest
      when is_cast arg1 ->
      Ast.SYMBOL((convert_dsymbol derefs)@
                 [mkparen arg1 known1;mkparen arg known])::
      loop rest
    | PAREN([EXPR(arg)],known)::PAREN([EXPR(arg1)],known1)::rest
      when is_cast arg1 ->
      Ast.SYMBOL([mkparen arg1 known1;mkparen arg known])::
      loop rest
    | PAREN([EXPR(arg)],known)::((PAREN(args,known1)::_) as rest)
      when not(List.length args = 1) ->
      Ast.SYMBOL([mkparen arg known])::(loop rest)
    | (PAREN(args,known)::rest) as l ->
      let (fn,rest) = find_symbol rest false in
      (match fn with
         None ->
         (match (args,known,rest) with
            ([EXPR(arg)],_,_) -> Ast.SYMBOL([mkparen arg known])::(loop rest)
          | ([],Ast.ENDUNKNOWN,_) ->
            Ast.SYMBOL([mkparen [] known])::(loop rest)
          | (_,_,[]) ->
            if no_fn_name_allowed
            then
              let known = make_front_unknown known in 
              Ast.CALL
                (Ast.SYMBOL([Ast.IDENT(Ast.bext ("",__unknown_line))]),
                 convert_codelist false args,known)::loop rest
            else
              failwith
                (Printf.sprintf "missing function name: %s %s"
                   (if rest = [] then "nothing"
                    else (unparse_expr (List.hd rest)))
                   (unparse_expr_list l))
          | (_,_,rest) ->
            match loop rest with
              fn::rest ->
              let new_args = convert_codelist false args in
              Ast.CALL(fn,new_args,known)::rest
            | _ ->
              failwith
                (Printf.sprintf "missing function name: %s %s"
                   (if rest = [] then "nothing"
                    else (unparse_expr (List.hd rest)))
                   (unparse_expr_list l)))
       | Some fn ->
         let new_args = convert_codelist false args in
         if rest = [] && function_declaration fn new_args
         then
           let (ty,annotations,name) = parse_function_name fn in
           Ast.PROTOTYPE(Ast.SYMBOL(fn),ty,annotations,name,new_args,known)
           ::loop rest
         else
           Ast.CALL(Ast.SYMBOL(fn),new_args,known)::loop rest)
    | expr::rest -> convert_expr expr :: loop rest in
  List.rev (loop backwards)


let convert c =
  (*Printf.eprintf "converting %s\n" (unparse_code_list c); flush stderr;*)
  top_convert_codelist true c
