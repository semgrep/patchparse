module CE = Ce
module Config = Globals
open Context_change
module CC = Context_change

let logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* When we have a - and a +, then we want to compare the two to omit the
   parts that are in common.  However, we want to maintain some context, so
   that we can identify similar things.  For example, if a function has a new
   argument, we want to keep the name of the function, so that we know that it
   is that function that has a new argument. *)

(* We really need to structure the changelist as a tree, and if we find
   friends for something higher in the tree, then we don't search for friends
   for things lower in the tree.  This is not implemented.  Currently
   normalize function call arguments.  Could also normalize dereferences and
   elements of code lists. *)

(* it is possible that there are only whitespace changes, and thus the two
   blocks turn out to be identical *)

type change =
    NO_CHANGE
  | CHANGE of CE.ce (* the rebuilt expr *) * change_type (* the changes *)
(* if CHANGE contains an IMMEDIATE_EXPR_CHANGE, then ce here should be the
   same as the ce of the IMMEDIATE_EXPR_CHANGE *)

and change_type =
  (* willing to merge with an expr or code context *)
    IMMEDIATE_EXPR_CHANGE of CE.ce (* the change *) * CC.t list
  (* only willing to merge with a code context, ie a code list *)
  | IMMEDIATE_CODE_CHANGE of CE.ce (* the change *) * CC.t list
  (* not willing to merge *)
  | CONTEXT_CHANGE of CC.t list

(*****************************************************************************)
(* Abstract line *)
(*****************************************************************************)

(* ----------------- abstract line diff ----------------------------- *)

let rec al_change = function
  | NO_CHANGE -> NO_CHANGE
  | CHANGE (ce (* the rebuilt expr *), change_type) (* the changes *) -> 
    CHANGE (Ce_al.al_ce ce, al_change_type change_type)
(* if CHANGE contains an IMMEDIATE_EXPR_CHANGE, then ce here should be the
   same as the ce of the IMMEDIATE_EXPR_CHANGE *)

and al_change_type = function
  (* willing to merge with an expr or code context *)
  | IMMEDIATE_EXPR_CHANGE (ce (* the change *) , context_change_list) -> 
    IMMEDIATE_EXPR_CHANGE (Ce_al.al_ce ce (* the change *),
                           List.map al_context_change context_change_list)
  (* only willing to merge with a code context, ie a code list *)
  | IMMEDIATE_CODE_CHANGE (ce (* the change *) , context_change_list) -> 
    IMMEDIATE_CODE_CHANGE (Ce_al.al_ce ce (* the change *),
                           List.map al_context_change context_change_list) 
  (* not willing to merge *)
  | CONTEXT_CHANGE context_change_list ->
    CONTEXT_CHANGE (List.map al_context_change context_change_list)

and al_context_change = function
  | CC ( ce (* the change *) , context_change_list) -> 
    CC (Ce_al.al_ce ce, List.map al_context_change context_change_list)
  | CG (ce (* a change generalized with EXP or CODE *), context_change_list) ->
    CG (Ce_al.al_ce ce, List.map al_context_change context_change_list)


(* ------------------------------------------------------------------------- *)
let rec have_al_change = function
  | NO_CHANGE -> false
  | CHANGE (ce (* the rebuilt expr *) , change_type) (* the changes *) -> 
    (Ce_al.have_al_ce ce || have_al_change_type change_type)

and have_al_change_type = function
  | IMMEDIATE_EXPR_CHANGE (ce (* the change *) , context_change_list) -> 
    (Ce_al.have_al_ce ce (* the change *) ||
     (List.exists have_al_context_change context_change_list))
  (* only willing to merge with a code context, ie a code list *)
  | IMMEDIATE_CODE_CHANGE (ce (* the change *) , context_change_list) -> 
    (Ce_al.have_al_ce ce (* the change *) ||
     (List.exists have_al_context_change context_change_list))
  (* not willing to merge *)
  | CONTEXT_CHANGE context_change_list ->
    (List.exists have_al_context_change context_change_list)

and have_al_context_change = function
  | CC (ce (* the change *) , context_change_list) -> 
    (Ce_al.have_al_ce ce ||
     (List.exists have_al_context_change context_change_list))
  | CG (ce (* a change generalized with EXP or CODE *), context_change_list) ->
    (Ce_al.have_al_ce ce ||
     (List.exists have_al_context_change context_change_list))


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* printing functions *)
let pr = Printf.sprintf

let context_printer context =
  let print_change change n =
    let pchange = Ce_unparse.ce2c change in
    logger#info "level %d: %s" n pchange 
  in
  let rec loop n = function
      [] -> ()
    | CC(change,context)::rest
    | CG(change,context)::rest ->
      print_change change n; loop (n+1) context; loop n rest in
  loop 0 context

let ct2c = function
    IMMEDIATE_EXPR_CHANGE(change,context) ->
    Printf.sprintf "immediate_expr(%s)" (Ce_unparse.ce2c_simple change)
  | IMMEDIATE_CODE_CHANGE(change,context) ->
    Printf.sprintf "immediate_code(%s)" (Ce_unparse.ce2c_simple change)
  | CONTEXT_CHANGE(changes)  ->
    Printf.sprintf "context_change %d" (List.length changes)

(* --------------------------------------------------------------------- *)
(* boring changes *)

let is_limited change =
  let is_field sym =
    List.exists
      (function Ast.SYMOP("->",_) | Ast.SYMOP(".",_) -> true | _ -> false)
      sym in
  let not_just_code = function
      Ast.CODE -> false
    | Ast.SEP(_) -> false
    | Ast.ARG(_) -> false
    | Ast.EXPR([Ast.SYMBOL([Ast.EXP(_)])]) -> false
    | _ -> true in
  let ok = function
      Ast.CALL(Ast.SYMBOL([Ast.IDENT(f,_)]),x,y) when
        let lid = String.lowercase_ascii f in
        Aux.substring "lock" lid ||
        Aux.substring "print" lid -> false
    | Ast.CALL(f1,x,y) -> true
    | Ast.DECLARER(f1,x,y) -> true
    | Ast.PROTOTYPE(_,_,_,_,_,_) -> true
    | Ast.ASSIGN(Ast.SYMBOL(lhs),_,_,_) -> is_field lhs
    | Ast.SYMBOL(s) -> is_field s
    | _ -> false in
  match change with
    CE.EXPRCE(e1,e2) ->
    (ok e1 || ok e2) &&
    (match (e1,e2) with
       (Ast.CALL(f1,a1,k1),Ast.CALL(f2,a2,k2)) ->
       (not (Ast_al.al_expr f1 = Ast_al.al_expr f2)) ||
       not(List.length a1 = List.length a2) ||
       List.exists not_just_code a1 || List.exists not_just_code a2
     |	(Ast.DECLARER(f1,a1,k1),Ast.DECLARER(f2,a2,k2)) ->
       (not (Ast_al.al_expr f1 = Ast_al.al_expr f2)) ||
       not(List.length a1 = List.length a2) ||
       List.exists not_just_code a1 || List.exists not_just_code a2
     |	_ -> true)
  | CE.EXPRLCE([],[e]) | CE.EXPRLCE([e],[]) -> ok e
  | CE.SYMCE(s1,s2) -> is_field s1 || is_field s2
  | _ -> false

let mkCC change context =
  if Ce_boring.boring_ce change
  then []
  else [CC(change,context)]

let mkCG change context =
  if Ce_boring.boring_ce change
  then []
  else [CG(change,context)]

(* --------------------------------------------------------------------- *)

let rec run_sequence_contexts prev n final = function
    [] -> final
  | mkchange::rest ->
    let change = mkchange n prev in
    if Ce_al.al_ce change = Ce_al.al_ce prev
    then run_sequence_contexts change n final rest
    else mkCG change (run_sequence_contexts change n final rest)

let sequence_contexts_final prev final contexts =
  run_sequence_contexts prev (ref 0) (*gensym ctr*) final contexts

let sequence_contexts prev contexts =
  run_sequence_contexts prev (ref 0) (*gensym ctr*) [] contexts

(* --------------------------------------------------------------------- *)
(* Normalize changes when downgrading them to context changes *)

(* Two problems can occur:

   The change can be the same as a context change just below it; sometimes
   both immediate and context changes are intentionally created for the
   same change.

   The change can contain unnecessary list nesting.  [x] and x always print
   the same, so we would like to treat them the same.  In particular, we
   always drop the outer [] on a downgraded change when both the minus and
   plus are lists of one element. *)

(* It is ok to use CC directly with an obviously interesting thing such as
   an assignment, but if the outer wrapping might be uninteresting, such as
   EXPR, then normalize_contextified_change should be used to avoid code that
   prints the same from being considered as different. *)

let rec normalize_contextified_change change context =
  match context with
    [CC(change1,context1)] when Ce_al.al_ce change = Ce_al.al_ce change1 -> context
  | _ ->
    (match change with
       CE.SYMCE([s1],[s2]) ->
       normalize_contextified_change (CE.PRIMCE(s1,s2)) context
     |	CE.EXPRCE(Ast.SYMBOL(s1),Ast.SYMBOL(s2)) ->
       normalize_contextified_change (CE.SYMCE(s1,s2)) context
     | CE.EXPRLCE([el1],[el2]) ->
       normalize_contextified_change (CE.EXPRCE(el1,el2)) context
     |	CE.CODECE(Ast.EXPR(el1),Ast.EXPR(el2)) ->
       normalize_contextified_change (CE.EXPRLCE(el1,el2)) context
     | CE.CODELCE([cl1],[cl2]) ->
       normalize_contextified_change (CE.CODECE(cl1,cl2)) context
     |	_ ->
       mkCC change
         ((sequence_contexts change [Ce_abstract.expify false; Ce_abstract.expify true]) @
          context))

(* --------------------------------------------------------------------- *)
(* Some useful functions *)

(* assumes that if we want expr changes, then they will be put in an expr
   context, and thus code changes become context changes *)
(* actually code changes that we might find here are currently always also
   context changes, so we could just drop them.  Instead we take a union. *)
let get_immediate_expr_changes n changelist =
  match changelist with
    IMMEDIATE_EXPR_CHANGE(change,context) -> (Some change,context)
  | IMMEDIATE_CODE_CHANGE(change,context) ->
    (None, normalize_contextified_change change context)
  | CONTEXT_CHANGE(changes) -> (None,changes)

let get_immediate_code_changes changelist =
  match changelist with
    IMMEDIATE_CODE_CHANGE(change,context) -> (Some change,context)
  | IMMEDIATE_EXPR_CHANGE(change,context) -> (Some change,context)
  | CONTEXT_CHANGE(changes) -> (None,changes)

let get_immediate_changes changelist =
  match changelist with
    IMMEDIATE_CODE_CHANGE(change,context) -> (Some change,None,context)
  | IMMEDIATE_EXPR_CHANGE(change,context) -> (None,Some change,context)
  | CONTEXT_CHANGE(changes) -> (None,None,changes)

let contextify changelist =
  match changelist with
    IMMEDIATE_CODE_CHANGE(change,context)
  | IMMEDIATE_EXPR_CHANGE(change,context) ->
    normalize_contextified_change change context
  | CONTEXT_CHANGE(changes) -> changes

let make_immediate_code_change change context =
  IMMEDIATE_CODE_CHANGE(change,context)

let make_immediate_expr_change change context =
  IMMEDIATE_EXPR_CHANGE(change,context)

(* duplicates can occur when an immediate with a context below it becomes
   a context *)
let make_context_change change context = 
  CONTEXT_CHANGE(normalize_contextified_change change context)

let process_unary_diff comparer t1 t2
    process_immediate get_immediate_changes combiner mkce =
  match comparer t1 t2 with
    NO_CHANGE ->
    let pi = process_immediate (mkce t1 t2) in
    CHANGE(pi,combiner pi [])
  | CHANGE(rebuilt,changelist) ->
    let pi = process_immediate rebuilt in
    let (immediate,context) = get_immediate_changes changelist in
    CHANGE(pi,
           match immediate with
             None -> combiner pi context
           | Some x -> combiner (process_immediate x) context)

let process_unary comparer t1 t2
    process_immediate get_immediate_changes combiner =
  match comparer t1 t2 with
    NO_CHANGE -> NO_CHANGE
  | CHANGE(rebuilt,changelist) ->
    let (immediate,context) = get_immediate_changes changelist in
    CHANGE(process_immediate rebuilt,
           match immediate with
             None -> CONTEXT_CHANGE(context)
           | Some x -> combiner (process_immediate x) context)

let inherit_unary_immediate_expr comparer t1 t2 process_immediate =
  process_unary comparer t1 t2 process_immediate
    (get_immediate_expr_changes 1)
    (function change -> function context ->
         make_immediate_expr_change change
           (normalize_contextified_change change context))
(* was just : make_immediate_expr_change *)

let inherit_unary_immediate_code comparer t1 t2 process_immediate =
  process_unary comparer t1 t2 process_immediate
    get_immediate_code_changes make_immediate_code_change

let contextify_unary_immediate comparer t1 t2 process_immediate =
  process_unary comparer t1 t2 process_immediate
    (get_immediate_expr_changes 2) make_context_change

let expr2code_unary_immediate comparer t1 t2 process_immediate =
  process_unary comparer t1 t2 process_immediate
    (get_immediate_expr_changes 3) make_immediate_code_change

(* Combine the left immediates with the right rebuilt, the right immediates
   with the left rebuilt, and the left immediates with the right immediates.
   In general, there should only be at most one immediate and it should be
   identical to the rebuilt, but we relax this for parentheses.  Union is used
   to combine the lists, because the immediate is generally the same as the
   rebuilt. *)
let pair_immediates lrebuilt rrebuilt left_immediate right_immediate =
  match (left_immediate,right_immediate) with
    (None,None) -> None
  | (Some left_immediate,None) -> Some (left_immediate,rrebuilt)
  | (None,Some right_immediate) -> Some (lrebuilt,right_immediate)
  | (Some left_immediate,Some right_immediate) ->
    Some (left_immediate,right_immediate)


let rec al_union_change xs ys = 
  let ys' = List.map al_context_change ys in
  let rec aux xs = 
    match xs with
      [] -> ys (* ys !!! and not ys' *)
    | x::xs ->
      if List.mem (al_context_change x) ys' then aux xs else x :: (aux xs) in
  aux xs

(* ------------------------------------------------------------------ *)
(* processing of a term with two subterms *)

let binary_result process_immediate get_immediate_changes combiner
    lrebuilt rrebuilt left_changelist right_changelist =
  let (left_immediate,left_context) =
    match left_changelist with
      None -> (None,[])
    | Some left_changelist -> get_immediate_changes left_changelist in
  let (right_immediate,right_context) =
    match right_changelist with
      None -> (None,[])
    | Some right_changelist -> get_immediate_changes right_changelist in
  match pair_immediates lrebuilt rrebuilt left_immediate right_immediate with
    None ->
    (* lists were combined with al_union_change, but they are from disjoint
       terms, so it's not clear why *)
    let contexts = left_context @ right_context in
    CHANGE(process_immediate(lrebuilt,rrebuilt),CONTEXT_CHANGE(contexts))
  | Some x ->
    CHANGE(process_immediate(lrebuilt,rrebuilt),
           combiner (process_immediate x) left_context right_context)

(* process_binary_separate_combiner: allows one to do something different
   with the sub changes found in the left and right subterms *)
let process_binary_separate_combiner lcomparer rcomparer mklce mkrce
    process_immediate get_immediate_changes combiner
    lt1 lt2 rt1 rt2 =
  let br = binary_result process_immediate get_immediate_changes combiner in
  match (lcomparer lt1 lt2,rcomparer rt1 rt2) with
    (NO_CHANGE,NO_CHANGE) -> NO_CHANGE
  | (NO_CHANGE,CHANGE(rrebuilt,right_changelist)) ->
    br (mklce lt1 lt2) rrebuilt None (Some right_changelist)
  | (CHANGE(lrebuilt,left_changelist),NO_CHANGE) ->
    br lrebuilt (mkrce rt1 rt2) (Some left_changelist) None
  | (CHANGE(lrebuilt,left_changelist),CHANGE(rrebuilt,right_changelist)) ->
    br lrebuilt rrebuilt (Some left_changelist) (Some right_changelist)

(* combines the changes found in the left and right subterms into a set *)
let process_binary lcomparer rcomparer mklce mkrce
    process_immediate get_immediate_changes combiner
    lt1 lt2 rt1 rt2 =
  let combined_combiner imm left_context right_context =
    (* lists were combined with al_union_change, but they are from disjoint
       terms, so it's not clear why *)
    let contexts = left_context @ right_context in 
    combiner imm contexts in
  process_binary_separate_combiner lcomparer rcomparer mklce mkrce
    process_immediate get_immediate_changes combined_combiner
    lt1 lt2 rt1 rt2

(* ------------------------------------------------------------------ *)

let process_list_element lcomparer rcomparer mklce mkrce
    process_immediate lt1 lt2 rt1 rt2 =
  match (lcomparer lt1 lt2,rcomparer rt1 rt2) with
    (NO_CHANGE,NO_CHANGE) -> NO_CHANGE
  | (NO_CHANGE,CHANGE(rrebuilt,right_changelist)) ->
    CHANGE(process_immediate (mklce lt1 lt2, rrebuilt),
           CONTEXT_CHANGE(contextify right_changelist))
  | (CHANGE(lrebuilt,left_changelist),NO_CHANGE) ->
    CHANGE(process_immediate (lrebuilt, mkrce rt1 rt2),
           CONTEXT_CHANGE(contextify left_changelist))
  | (CHANGE(lrebuilt,left_changelist),CHANGE(rrebuilt,right_changelist)) ->
    CHANGE(process_immediate (lrebuilt, rrebuilt),
           CONTEXT_CHANGE(contextify left_changelist @
                          contextify right_changelist))

(* immediate should be the same as rebuilt, for context, it might be *)
let top_level_diff defaultce combiner = function
    NO_CHANGE -> CHANGE(defaultce,combiner defaultce [])
  | CHANGE(rebuilt,changelist) ->
    (match changelist with
       IMMEDIATE_CODE_CHANGE(change,context)
     | IMMEDIATE_EXPR_CHANGE(change,context) ->
       CHANGE(rebuilt,combiner rebuilt context)
     | CONTEXT_CHANGE(changes) ->
       let changes =
         match changes with
           [CC(change1,context1)]
           when Ce_al.al_ce rebuilt = Ce_al.al_ce change1 -> context1
         | _ -> changes in
       CHANGE(rebuilt,combiner rebuilt changes))

(* immediate is not the same as rebuilt *)
let top_level_diff_diff defaultce combiner = function
    NO_CHANGE -> CHANGE(defaultce,combiner defaultce [])
  | CHANGE(rebuilt,changelist) ->
    CHANGE(defaultce,combiner defaultce (contextify changelist))

(* --------------------------------------------------------------------- *)
(* treatment of lists *)

(* this require that combiner make an immediate change, not a context change *)
(* should have this at all levels *)
let compare_both_length_one comparer mkrce l1 l2 process_immediate =
  match comparer l1 l2 with
    CHANGE(lrebuilt,left_changelist) ->
    (match get_immediate_changes left_changelist with
       (Some code_change,None,context) ->
       CHANGE(process_immediate(lrebuilt,mkrce [] []),
              IMMEDIATE_CODE_CHANGE(process_immediate
                                      (code_change,mkrce [] []),
                                    context))
     | (None,Some expr_change,context) ->
       CHANGE(process_immediate(lrebuilt,mkrce [] []),
              IMMEDIATE_EXPR_CHANGE(process_immediate
                                      (expr_change,mkrce [] []),
                                    context))
     | (None,None,context) ->
       CHANGE(process_immediate(lrebuilt,mkrce [] []),
              CONTEXT_CHANGE(context))
     | _ -> failwith "not possible 1")
  | _ -> failwith (Printf.sprintf "not possible 1 no change %s"
                     (Ce_unparse.ace2c(mkrce [l1] [l2])))

(*let eq_aligner ((x,_),(y,_)) = x = y*)
(*let eq_aligner (x,y) = x = y*)

(* separate_dropped should be true when get_everything is false, but may be
   false when get_everything is true.  In particular, separate_dropped is false
   for symbols, because we just want to keep them together, and true for
   function calls, because we want to know about added and dropped things and this
   is our only opportunity, even though we want to group together the whole
   argument list as well. *)
let compare_lists get_everything separate_dropped
    comparer comparer2 mklce mkrce l1 l2 process_immediate
    process_immediate_lists organize get_immediate_changes combiner =

  let comparer3 (x,y) =
    List.length x = List.length y &&
    (List.for_all (fun (x,y) -> comparer2 (x,y)) (List.combine x y)) in
  let comparer4 (x,y) =
    List.length x = List.length y &&
    (List.for_all (fun (x,y) -> comparer3 (x,y)) (List.combine x y)) in
  let comparer5 (x,y) =
    List.length x = List.length y &&
    (List.for_all (fun (x,y) -> comparer4 (x,y)) (List.combine x y)) in


  match (l1,l2) with
    ([l1],[l2]) when not (comparer2 (l1, l2)) ->
    (* a special case where both lists have length 1; in this case
       	 we just want to inherit the properties of the lists, not use
       	 combiner *)
    compare_both_length_one comparer mkrce l1 l2 process_immediate
  | _ ->
    let (organized_l1,organized_l2) = organize (l1,l2) in
    let mkrce2 l1 l2 = (* triply nested elements *)
      mkrce 
        (List.concat (List.map List.concat l1))
        (List.concat (List.map List.concat l2)) in
    let mkrce1 l1 l2 = (* doubly nested elements *)
      mkrce (List.concat l1) (List.concat l2) in
    let rec loop_maker comparer flatten mklce mkrce process_immediate
        inner_loop =
      let rec loop l1 l2 =
        match (l1,l2) with
          (l1,l2) when comparer (l1, l2) -> NO_CHANGE (* HERE *)
        | ([],rest) ->
          let entry = mkrce [] rest in
          if get_everything && not separate_dropped
          then CHANGE(entry,combiner entry [])
          else
            CHANGE(entry,combiner entry
                     (mkCC entry
                        (sequence_contexts entry
                           [Ce_abstract.expify false;Ce_abstract.mktail;Ce_abstract.expify true])))
        | (rest,[]) ->
          let entry = mkrce rest [] in
          if get_everything && not separate_dropped
          then CHANGE(entry,combiner entry [])
          else
            CHANGE(entry,combiner entry
                     (mkCC entry
                        (sequence_contexts entry
                           [Ce_abstract.expify false;Ce_abstract.mktail;Ce_abstract.expify true])))
        | (e1::rest1,e2::rest2) -> 
          if flatten
          then
            (* attaches car to cdr, then combines *)
            process_binary inner_loop loop mklce mkrce process_immediate
              get_immediate_changes combiner e1 e2 rest1 rest2
          else
            (* combines, then attaches car to cdr, always contextifies *)
            process_list_element inner_loop loop mklce mkrce
              process_immediate e1 e2 rest1 rest2 in
      loop in
    let innermost_loop =
      loop_maker comparer3 true mklce mkrce process_immediate comparer in
    let middle_loop = 
      loop_maker comparer4 true mkrce mkrce1 process_immediate_lists
        innermost_loop in
    let outer_loop =
      loop_maker comparer5 get_everything mkrce1 mkrce2
        process_immediate_lists middle_loop in
    outer_loop organized_l1 organized_l2

(****************************************************************************)
(* Compare ASTs *)
(****************************************************************************)

(* --------------------------------------------------------------------- *)
(* treatment of primitives *)

let rec compare_prim prim1 prim2 =
  if Ast_al.al_prim prim1 = Ast_al.al_prim prim2
  then NO_CHANGE
  else
    match (prim1,prim2) with
      (Ast.ARRAY(expr1,known1),Ast.ARRAY(expr2,known2)) ->
      let process_immediate = function
          CE.EXPRLCE(el1,el2) ->
          CE.PRIMCE(Ast.ARRAY(el1,known1),Ast.ARRAY(el2,known2))
        | _ -> failwith "not possible 3" in
      let res = contextify_unary_immediate compare_expr_lists expr1 expr2
          process_immediate in
      if known1 = known2
      then res
      else
        top_level_diff (CE.PRIMCE(prim1,prim2)) make_immediate_expr_change res
    | (Ast.PARENSYM(expr1,known1),Ast.PARENSYM(expr2,known2))
      when known1 = known2 ->
      let process_immediate = function
          CE.EXPRLCE(el1,el2) ->
          CE.PRIMCE(Ast.PARENSYM(el1,known1),Ast.PARENSYM(el2,known2))
        | _ -> failwith "not possible 5" in
      let res = contextify_unary_immediate compare_expr_lists expr1 expr2
          process_immediate in
      if known1 = Ast.KNOWN && known2 = Ast.KNOWN
      then res
      else
        top_level_diff (CE.PRIMCE(prim1,prim2)) make_immediate_expr_change res
    | (prim1,prim2) ->
      let rebuilt = CE.PRIMCE(prim1,prim2) in
      CHANGE(rebuilt,make_immediate_expr_change rebuilt [])

and compare_prim_lists l1 l2 =
  compare_lists true false compare_prim
    (fun (x,y) -> Ast_al.al_prim x = Ast_al.al_prim y)
    (function x -> function y -> CE.PRIMCE(x,y))
    (function x -> function y -> CE.SYMCE(x,y))
    l1 l2
    (function
        (CE.PRIMCE(s1,s2),CE.SYMCE(sl1,sl2)) -> CE.SYMCE(s1::sl1,s2::sl2)
      |	_ -> failwith "not possible 7")
    (function
        (CE.SYMCE(s1,s2),CE.SYMCE(sl1,sl2)) -> CE.SYMCE(s1@sl1,s2@sl2)
      |	_ -> failwith "not possible 8")
    (Small.top
       (function Ast.SYMOP(_) -> true | _ -> false)
       (function Ast.SYMOP(_) -> true | _ -> false)
       [(fun (x,y) -> Ast_al.al_prim x = Ast_al.al_prim y)]
       (function Ast.SYMOP("&",_) | Ast.SYMOP("*",_) -> true | _ -> false))
    (get_immediate_expr_changes 4)
    make_immediate_expr_change

(* --------------------------------------------------------------------- *)
(* treatment of symbols *)

(* If the first identifier changes, then it is an immediate change.  If not
   and only the last identifier changes, then it is both an immediate change
   and a context change (not sure if the field is renamed or we want different
   information).  If not and an identifier in the middle changes, then we
   assume that it is a context change, ie that the information obtained is the
   same, but we get it in a different way.  Everything else that has a
   difference is an immediate change, whereever the change occurs. *)

and compare_symbol prims1 prims2 =
  if List.length prims1 = List.length prims2 && 
     List.for_all (fun (prim1, prim2) -> Ast_al.al_prim prim1 = Ast_al.al_prim prim2)
       (List.combine prims1 prims2)
  then NO_CHANGE
  else
    let first_id_same = ref false in
    let last_id_different = ref false in
    let last_id_same = ref false in
    let rec find_first_id = function
        (Ast.IDENT(id1,_)::rest1,Ast.IDENT(id2,_)::rest2) ->
        Some(id1=id2,rest1,rest2)
      | (Ast.PARENSYM(expr1,known1)::rest1,
         Ast.PARENSYM(expr2,known2)::rest2) ->
        Some((List.map Ast_al.al_expr expr1)=(List.map Ast_al.al_expr expr2)&&
             known1=known2,rest1,rest2)
      | (Ast.ARRAY(_,_)::rest1,Ast.ARRAY(_,_)::rest2) ->
        find_first_id (rest1,rest2)
      | (x::rest1,y::rest2)
        when Ast_al.al_prim x = Ast_al.al_prim y -> find_first_id (rest1,rest2)
      | _ -> None in
    (match find_first_id (prims1,prims2) with
       Some(same_start_id, rest1, rest2) ->
       if same_start_id then first_id_same := true;
       (match find_first_id (List.rev rest1,List.rev rest2) with
          Some(same_end_id, middle1, middle2) ->
          if same_end_id
          then last_id_same := true
          else last_id_different := true;
        | _ -> ())
     | _ -> ());
    match compare_prim_lists prims1 prims2 with
      NO_CHANGE -> NO_CHANGE (* the same, so no point for extra_context *)
    | CHANGE(rebuilt,changelist) as x ->
      (match get_immediate_expr_changes 5 changelist with
         (Some change,context) ->
         CHANGE(rebuilt,
                if !first_id_same
                then
                  if !last_id_different
                  then
                    let contexts =
                      (sequence_contexts rebuilt
                         [Ce_abstract.expify false;Ce_abstract.expify true])@
                      context in
                    IMMEDIATE_EXPR_CHANGE(rebuilt,
                                          (mkCC rebuilt contexts))
                  else
                  if !last_id_same
                  then
                    let contexts =
                      (sequence_contexts rebuilt
                         [Ce_abstract.expify false;Ce_abstract.expify true])@
                      context in
                    CONTEXT_CHANGE(mkCC rebuilt contexts)
                  else changelist (* no last id *)
                else changelist)
       | (None,context) -> x)

(* --------------------------------------------------------------------- *)
(* treatment of expressions *)

and compare_expr e1 e2 =
  if Ast_al.al_expr e1 = Ast_al.al_expr e2
  then NO_CHANGE
  else
    match (e1,e2) with
      (Ast.SYMBOL(sym1),Ast.SYMBOL(sym2)) ->
      let process_immediate = function
          CE.SYMCE(rs1,rs2) -> CE.EXPRCE(Ast.SYMBOL(rs1),Ast.SYMBOL(rs2))
        | _ -> failwith "not possible 8" in
      inherit_unary_immediate_expr compare_symbol sym1 sym2 process_immediate
    (* all variants of assignments *)
    | (Ast.ASSIGN(lhs1,op1,rhs1,Ast.KNOWN),
       Ast.ASSIGN(lhs2,op2,rhs2,Ast.KNOWN)) ->
      let process_immediate = function
          (CE.EXPRCE(s1,s2),CE.EXPRLCE(e1,e2)) ->
          CE.EXPRCE(Ast.ASSIGN(s1,op1,e1,Ast.KNOWN),
                    Ast.ASSIGN(s2,op2,e2,Ast.KNOWN))
        | _ -> failwith "not possible 9" in
      let res =
        process_binary compare_expr compare_expr_lists
          (function x -> function y -> CE.EXPRCE(x,y))
          (function x -> function y -> CE.EXPRLCE(x,y))
          process_immediate
          (get_immediate_expr_changes 7)
          (function change -> function context ->
               make_immediate_code_change change
                 (mkCC change
                    ((sequence_contexts change [Ce_abstract.expify false])@context)))
          lhs1 lhs2 rhs1 rhs2 in
      if fst op1 = fst op2 (* PAD *)
      then res
      else top_level_diff (CE.EXPRCE(e1,e2)) make_immediate_code_change res
    | (Ast.ASSIGN(lhs1,op1,[],Ast.ENDUNKNOWN),
       Ast.ASSIGN(lhs2,op2,[],Ast.ENDUNKNOWN)) ->
      (* since we don't have the right hand side, there is no context *)
      let process_immediate = function
          CE.EXPRCE(s1,s2) ->
          CE.EXPRCE(Ast.ASSIGN(s1,op1,[],Ast.ENDUNKNOWN),
                    Ast.ASSIGN(s2,op2,[],Ast.ENDUNKNOWN))
        | _ -> failwith "not possible 10" in
      let res =
        expr2code_unary_immediate compare_expr lhs1 lhs2 process_immediate in
      top_level_diff (CE.EXPRCE(e1,e2)) make_immediate_code_change res
    | (Ast.ASSIGN(lhs1,op1,rhs1,Ast.FRONTUNKNOWN),
       Ast.ASSIGN(lhs2,op2,rhs2,Ast.FRONTUNKNOWN)) ->
      (* since we don't have the right hand side, there is no context *)
      let process_immediate = function
          CE.EXPRLCE(e1,e2) ->
          CE.EXPRCE(Ast.ASSIGN(lhs1,op1,e1,Ast.FRONTUNKNOWN),
                    Ast.ASSIGN(lhs2,op2,e2,Ast.FRONTUNKNOWN))
        | _ -> failwith "not possible 10" in
      let res =
        expr2code_unary_immediate compare_expr_lists rhs1 rhs2 
          process_immediate in
      top_level_diff (CE.EXPRCE(e1,e2)) make_immediate_code_change res
    | (Ast.ASSIGN(lhs1,op1,rhs1,known1),Ast.ASSIGN(lhs2,op2,rhs2,known2)) ->
      (* one is unknown *)
      (* we consider that there is an immediate change on the right hand
         	   side *)
      (* doesn't matter whether the ops are the same, because at the top
         	level the terms are already different *)
      let process_immediate = function
          CE.EXPRCE(s1,s2) ->
          CE.EXPRCE(Ast.ASSIGN(s1,op1,rhs1,known1),
                    Ast.ASSIGN(s2,op2,rhs2,known2))
        | _ -> failwith "not possible 11" in
      let res =
        expr2code_unary_immediate compare_expr lhs1 lhs2 process_immediate in
      top_level_diff (CE.EXPRCE(e1,e2)) make_immediate_code_change res
    | (Ast.ASSIGN(lhs1,op1,rhs1,known1),expr2) ->
      let process_immediate = function
          CE.EXPRLCE(el1,[el2]) ->
          CE.EXPRCE(Ast.ASSIGN(lhs1,op1,el1,known1),el2)
        | _ -> failwith "not possible 10a" in
      process_unary_diff compare_expr_lists rhs1 [expr2]
        process_immediate get_immediate_code_changes
        (function change -> function context ->
             make_immediate_code_change change
               (mkCC change
                  ((sequence_contexts change [Ce_abstract.expify false])@context)))
        (function e1 -> function e2 -> CE.EXPRLCE(e1,e2))
    | (expr1,Ast.ASSIGN(lhs2,op2,rhs2,known2)) ->
      let process_immediate = function
          CE.EXPRLCE([el1],el2) ->
          CE.EXPRCE(el1,Ast.ASSIGN(lhs2,op2,el2,known2))
        | _ -> failwith "not possible 10b" in
      process_unary_diff compare_expr_lists [expr1] rhs2
        process_immediate get_immediate_code_changes
        (function change -> function context ->
             make_immediate_code_change change
               (mkCC change
                  ((sequence_contexts change [Ce_abstract.expify false])@context)))
        (function e1 -> function e2 -> CE.EXPRLCE(e1,e2))
    (* all variants of function calls *)
    | (Ast.CALL(fn1,args1,known1),Ast.CALL(fn2,args2,known2)) ->
      let process_immediate = function
          (CE.EXPRCE(s1,s2),CE.CODELCE(cl1,cl2)) ->
          CE.EXPRCE(Ast.CALL(s1,cl1,known1),Ast.CALL(s2,cl2,known2))
        | _ -> failwith "not possible 12" in
      (* even if the change is deeply nested in the arguments, we want to
         	   see the whole function call, in case this complete change occurs
         	   in many cases *)
      let res =
        compare_calls process_immediate fn1 fn2 args1 args2 known1 known2 in
      top_level_diff (CE.EXPRCE(e1,e2)) make_immediate_code_change res
    | (Ast.DECLARER(fn1,args1,known1),Ast.DECLARER(fn2,args2,known2)) ->
      let process_immediate = function
          (CE.EXPRCE(s1,s2),CE.CODELCE(cl1,cl2)) ->
          CE.EXPRCE(Ast.DECLARER(s1,cl1,known1),Ast.DECLARER(s2,cl2,known2))
        | _ -> failwith "not possible 12" in
      (* even if the change is deeply nested in the arguments, we want to
         	   see the whole function call, in case this complete change occurs
         	   in many cases *)
      let res =
        compare_calls process_immediate fn1 fn2 args1 args2 known1 known2 in
      top_level_diff (CE.EXPRCE(e1,e2)) make_immediate_code_change res
    (* all variants of function prototypes *)
    | (Ast.PROTOTYPE(fn1,ty1,vis1,nm1,args1,known1),
       Ast.PROTOTYPE(fn2,ty2,vis2,nm2,args2,known2)) ->
      let process_immediate = function
          (CE.EXPRCE(s1,s2),CE.CODELCE(cl1,cl2)) ->
          CE.EXPRCE(Ast.PROTOTYPE(s1,ty1,vis1,nm1,cl1,known1),
                    Ast.PROTOTYPE(s2,ty2,vis2,nm2,cl2,known2))
        | _ -> failwith "not possible 12" in
      (* even if the change is deeply nested in the arguments, we want to
         	   see the whole function call, in case this complete change occurs
         	   in many cases *)
      let res =
        compare_calls process_immediate fn1 fn2 args1 args2 known1 known2 in
      top_level_diff (CE.EXPRCE(e1,e2)) make_immediate_code_change res
    (* all variants of parentheses *)
    (* It would be nice that parentheses are invisible; that is that
       if there is an immediate change with parentheses, then there is an
       immediate change without parentheses.  But with parentheses we have
       an expr and without we have an expr list, so the types don't work
       out.  This could perhaps be fixed, but does not seem like a
       priority. *)
    | (Ast.SYMBOL([Ast.PARENSYM(expr1,known1)]),expr2) ->
      let process_immediate = function
          CE.EXPRLCE(e1,[e2]) ->
          CE.EXPRCE(Ast.SYMBOL([Ast.PARENSYM(e1,known1)]),e2)
        | _ -> failwith "not possible 14" in
      top_level_diff (CE.EXPRCE(e1,e2)) make_immediate_expr_change
        (inherit_unary_immediate_expr compare_expr_lists expr1 [expr2]
           process_immediate)
    | (expr1,Ast.SYMBOL([Ast.PARENSYM(expr2,known2)])) ->
      let process_immediate = function
          CE.EXPRLCE([e1],e2) ->
          CE.EXPRCE(e1,Ast.SYMBOL([Ast.PARENSYM(e2,known2)]))
        | _ -> failwith "not possible 15" in
      top_level_diff (CE.EXPRCE(e1,e2)) make_immediate_expr_change
        (inherit_unary_immediate_expr compare_expr_lists [expr1] expr2
           process_immediate)
    (* all variants of structures *)
    | (Ast.STRUCT(fields1,known1),Ast.STRUCT(fields2,known2)) ->
      let process_immediate = function
          CE.CODELCE(cl1,cl2) ->
          CE.EXPRCE(Ast.STRUCT(cl1,known1),Ast.STRUCT(cl2,known2))
        | _ -> failwith "not possible 17" in
      let res =
        inherit_unary_immediate_expr compare_code_lists fields1 fields2
          process_immediate in
      if known1 = known2
      then res
      else top_level_diff (CE.EXPRCE(e1,e2)) make_context_change res
    (* nothing in common *)
    | _ -> CHANGE(CE.EXPRCE(e1,e2),
                  make_immediate_expr_change (CE.EXPRCE(e1,e2)) [])

(* --------------------------------------------------------------------- *)
(* Function calls *)

(* This is kind of a hack.  We first normalize the argument lists by
   replacing common arguments by ARGn, where n is the position of the
   argument, which may differ between the two calls.  We then call
   process_binary in the usual way.  The ARGn things will either be identical
   or will be seen as top level changes, which will cause the whole function
   call to be seen as a change.  On the other hand, if the whole call turns
   out to be identical (but how could it?), the result will be NO_CHANGE and
   the ARGn terms will disappear. *)

(* Note that separators should normally match up.  The only case in which
   they won't is a for loop where some of the components are missing.  In this
   case, we could consider whether we should favor matches on terms over
   matches on separators, or vice versa.  For the moment, we do not make the
   distinction.  Actually, based on the way for is used, we would rather favor
   the separator in this case. Note that the argument count includes the
   separators, which is unfamiliar, but doesn't make any difference, and is
   convenient because we don't have to count the separators when we skip over
   part of the sequence. *)

and index  (x: Ast.code) l =
  let rec loop = function
      [] -> raise Not_found
    | (y::rest) ->
      if Ast_al.al_code x = Ast_al.al_code y then 0 else 1 + loop rest in
  try Some(loop l) with Not_found -> None

and split l n =
  let rec loop = function
      (0,x::rest) -> ([],rest)
    | (n,x::rest) ->
      let (pref,rest) = loop (n-1, rest) in
      (x::pref,rest)
    | _ -> failwith "not possible: split" in
  loop (n,l)

and compare_calls process_immediate fn1 fn2 args1 args2 known1 known2 =
  let argify n = function
      Ast.EXPR(exprlist) -> Ast.ARG(n)
    | x -> x in
  let rec loop ctr = function (* align arguments *)
      ([],_) | (_,[]) as x -> x
    | (x::rest1,y::rest2) ->
      if Ast_al.al_code x = Ast_al.al_code y
      then
        let (new_rest1,new_rest2) = loop (ctr+1) (rest1,rest2) in
        ((argify ctr x)::new_rest1,(argify ctr y)::new_rest2)
      else
        let pickx xindex =
          let (pref2(*<xindex*),rest2(*>xindex*)) = split rest2 xindex in
          (*let xindex = xindex + 1 in*) (* because from the list tail *)
          let (new_rest1,new_rest2) = loop (ctr+1) (rest1,rest2) in
          ((argify ctr x)::new_rest1,
           y::pref2@(argify ctr x)::new_rest2) in
        let picky yindex =
          let (pref1(*<yindex*),rest1(*>yindex*)) = split rest1 yindex in
          (*let yindex = yindex + 1 in*) (* because from the list tail *)
          let (new_rest1,new_rest2) = loop (ctr+1) (rest1,rest2) in
          (x::pref1@(argify ctr y)::new_rest1,
           (argify ctr y)::new_rest2) in
        (match (index x rest2,index y rest1) with
           (None,None) ->
           let (new_rest1,new_rest2) = loop ctr (rest1,rest2) in
           (x::new_rest1,y::new_rest2)
         | (None,Some yindex) -> picky yindex
         | (Some xindex, None) -> pickx xindex
         | (Some xindex, Some yindex) ->
           if xindex < yindex
           then pickx xindex
           else picky yindex) in
  let (new_args1,new_args2) = loop 0 (args1,args2) in
  process_binary_separate_combiner compare_expr compare_code_lists
    (function x -> function y -> CE.EXPRCE(x,y))
    (function x -> function y -> CE.CODELCE(x,y))
    process_immediate
    (get_immediate_expr_changes 8)
    (function change -> function left_context -> function right_context ->
       match change with
         CE.EXPRCE(Ast.CALL(fn1,args1,known1),Ast.CALL(fn2,args2,known2)) ->
         make_immediate_code_change change
           (mkCC change
              ((sequence_contexts_final change left_context
                  [Ce_abstract.expify false;Ce_abstract.codify_expr_change;Ce_abstract.codify_all_args])@
               right_context))
       |	CE.EXPRCE(Ast.DECLARER(fn1,args1,known1),Ast.DECLARER(fn2,args2,known2)) ->
         make_immediate_code_change change
           (mkCC change
              ((sequence_contexts_final change left_context
                  [Ce_abstract.expify false;Ce_abstract.codify_expr_change;Ce_abstract.codify_all_args])@
               right_context))
       |	CE.EXPRCE(Ast.PROTOTYPE(fn1,ty1,vis1,nm1,args1,known1),
                   Ast.PROTOTYPE(fn2,ty2,vis2,nm2,args2,known2)) ->
         (* left context is the function name and all modifiers, but
            	     if we don't have the right number of arguments with it, who
            	     cares.  The problem is that Ce_abstract.expify is going to drop the
            	     modifiers, which will make the last result of sequence_contexts
            	     smaller than the beginning of left_context, if the
            	     function has modifiers like static or inline.  We could drop
            	     these from left_context, but left_context doesn't seem so useful
            	     anyway *)
         make_immediate_code_change change
           (mkCC change
              ((sequence_contexts_final change [] (*left_context*)
                  [Ce_abstract.expify false; Ce_abstract.expify true]) @
               right_context))
       |	_ -> failwith "not possible")
    fn1 fn2 new_args1 new_args2

(* --------------------------------------------------------------------- *)
(* Other terms *)

and compare_expr_lists l1 l2 =
  compare_lists false true compare_expr
    (fun (x,y) -> Ast_al.al_expr x = Ast_al.al_expr y)
    (function x -> function y -> CE.EXPRCE(x,y))
    (function x -> function y -> CE.EXPRLCE(x,y))
    l1 l2
    (function
        (CE.EXPRCE(e1,e2),CE.EXPRLCE(el1,el2)) -> CE.EXPRLCE(e1::el1,e2::el2)
      |	(x,y) ->
        failwith
          (pr "not possible 18: %s %s" (Ce_unparse.ce2label x) (Ce_unparse.ce2label y)))
    (function
        (CE.EXPRLCE(e1,e2),CE.EXPRLCE(el1,el2)) -> CE.EXPRLCE(e1@el1,e2@el2)
      |	(x,y) ->
        failwith
          (pr "not possible 19: %s %s" (Ce_unparse.ce2label x) (Ce_unparse.ce2label y)))
    (Small.top
       (function Ast.EOP(_) -> true | _ -> false)
       (function Ast.EOP(_) -> true | _ -> false)
       [(fun (x,y) -> Ast_al.al_expr x = Ast_al.al_expr y);
        (function
            (Ast.CALL(_,_,_),Ast.CALL(_,_,_)) -> true
          | (Ast.DECLARER(_,_,_),Ast.DECLARER(_,_,_)) -> true
          | (Ast.PROTOTYPE(_,_,_,_,_,_),Ast.PROTOTYPE(_,_,_,_,_,_)) -> true
          | (Ast.ASSIGN(_,_,_,_),Ast.ASSIGN(_,_,_,_)) -> true
          | _ -> false)]
       (function Ast.EOP("!",_) | Ast.EOP("~",_) -> true | _ -> false))
    (get_immediate_expr_changes 9) make_immediate_expr_change
(* was (make_context_change 4) *)

and compare_code c1 c2 =
  if Ast_al.al_code c1 = Ast_al.al_code c2
  then NO_CHANGE
  else
    match (c1,c2) with
      (Ast.EXPR(expr1),Ast.EXPR(expr2)) ->
      let process_immediate = function
          CE.EXPRLCE(e1,e2)  -> CE.CODECE(Ast.EXPR(e1),Ast.EXPR(e2))
        | _ -> failwith "not possible 20" in
      inherit_unary_immediate_expr
        compare_expr_lists expr1 expr2 process_immediate
    | _ ->CHANGE(CE.CODECE(c1,c2),
                 make_immediate_expr_change(CE.CODECE(c1,c2)) [])

and compare_toplevel c1 c2 =
  if Ast_al.al_code c1 = Ast_al.al_code c2
  then NO_CHANGE
  else
    match (c1,c2) with
      (Ast.EXPR(expr1),Ast.EXPR(expr2)) ->
      let process_immediate = function
          CE.EXPRLCE(e1,e2) -> CE.CODECE(Ast.EXPR(e1),Ast.EXPR(e2))
        | _ -> failwith "not possible 21" in
      inherit_unary_immediate_code
        compare_expr_lists expr1 expr2 process_immediate
    | _ ->
      CHANGE(CE.CODECE(c1,c2),
             make_immediate_expr_change (CE.CODECE(c1,c2)) [])


(* The first argument to compare_lists has to be false so that the argument
   list gets collected up with the function when there are differences in the
   argument list, eg from f(a,b) to f(a,y).  But for some reason at some point
   we also wanted the opposite behavior value, true, which would seem to have
   the effect of not accumulating the arguments.  It seems that this is so
   that arguments that are added or dropped appear separately.  But it seems
   better to do that explicitly in compare_lists. *)
and compare_code_lists l1 l2 =
  compare_lists true true compare_code
    (fun (x,y) -> Ast_al.al_code x = Ast_al.al_code y)
    (function x -> function y -> CE.CODECE(x,y))
    (function x -> function y -> CE.CODELCE(x,y))
    l1 l2
    (function
        (CE.CODECE(c1,c2),CE.CODELCE(cl1,cl2)) -> CE.CODELCE(c1::cl1,c2::cl2)
      |	_ -> failwith "not possible 22")
    (function
        (CE.CODELCE(c1,c2),CE.CODELCE(cl1,cl2)) -> CE.CODELCE(c1@cl1,c2@cl2)
      |	_ -> failwith "not possible 23")
    (Small.top
       (function Ast.SEP(";",_) | Ast.SEP(",",_) -> true | _ -> false)
       (function _ -> false)
       [function (x,y) -> Ast_al.al_code x = Ast_al.al_code y]
       (function _ -> false))
    (get_immediate_expr_changes 10) make_immediate_expr_change

and compare_toplevel_lists l1 l2 =
  compare_lists false true compare_toplevel
    (function (x,y) -> Ast_al.al_code x = Ast_al.al_code y)
    (function x -> function y -> CE.CODECE(x,y))
    (function x -> function y -> CE.CODELCE(x,y))
    l1 l2
    (function
        (CE.CODECE(c1,c2),CE.CODELCE(cl1,cl2)) -> CE.CODELCE(c1::cl1,c2::cl2)
      |	_ -> failwith "not possible 24")
    (function
        (CE.CODELCE(c1,c2),CE.CODELCE(cl1,cl2)) -> CE.CODELCE(c1@cl1,c2@cl2)
      |	_ -> failwith "not possible 25")
    (Small.top
       (function Ast.SEP(";",_) -> true | _ -> false)
       (function _ -> false)
       [(function (x,y) -> Ast_al.al_code x = Ast_al.al_code y);
        (function
            (Ast.EXPR([Ast.CALL(fn1,_,_)]),
             Ast.EXPR([Ast.CALL(fn2,_,_)])) ->
            Ast_al.al_expr fn1 = Ast_al.al_expr fn2
          | (Ast.EXPR([Ast.DECLARER(fn1,_,_)]),
             Ast.EXPR([Ast.DECLARER(fn2,_,_)])) ->
            Ast_al.al_expr fn1 = Ast_al.al_expr fn2
          | (Ast.EXPR([Ast.PROTOTYPE(_,_,_,nm1,_,_)]),
             Ast.EXPR([Ast.PROTOTYPE(_,_,_,nm2,_,_)])) -> nm1=nm2
          | (Ast.EXPR([Ast.ASSIGN(e1,_,_,_)]),
             Ast.EXPR([Ast.ASSIGN(e2,_,_,_)])) ->
            Ast_al.al_expr e1 = Ast_al.al_expr e2
          | _ -> false);
        (function
            (Ast.EXPR([Ast.CALL(fn1,_,_)]),
             Ast.EXPR([Ast.CALL(fn2,_,_)])) -> Config2.real_fn fn1 = Config2.real_fn fn2
          | (Ast.EXPR([Ast.DECLARER(fn1,_,_)]),
             Ast.EXPR([Ast.DECLARER(fn2,_,_)])) ->
            Config2.real_fn fn1 = Config2.real_fn fn2
          | (Ast.EXPR([Ast.ASSIGN(e1,_,_,_)]),
             Ast.EXPR([Ast.CALL(fn2,_,_)])) -> Config2.memory_mover fn2
          | (Ast.EXPR([Ast.CALL(fn1,_,_)]),
             Ast.EXPR([Ast.ASSIGN(e2,_,_,_)])) -> Config2.memory_mover fn1
          | (Ast.EXPR([Ast.PROTOTYPE(_,_,_,nm1,_,_)]),
             Ast.EXPR([Ast.PROTOTYPE(_,_,_,nm2,_,_)])) -> nm1=nm2
          | (Ast.EXPR([Ast.ASSIGN(_,_,_,_)]),
             Ast.EXPR([Ast.ASSIGN(_,_,_,_)])) -> true
          | _ -> false)]
       (function _ -> false))
    get_immediate_code_changes make_immediate_code_change


and compare_toplevel_lists_lists l1 l2 =
  let (organized_l1,organized_l2) =
    Small.top
      (function _ -> false)
      (function _ -> false)
      [(function (x,y) -> List.map Ast_al.al_code x = List.map Ast_al.al_code y);
       (function
           ((Ast.EXPR([Ast.PROTOTYPE(_,_,_,nm1,_,_)])::_),
            (Ast.EXPR([Ast.PROTOTYPE(_,_,_,nm2,_,_)])::_)) -> nm1=nm2
         | _ -> false)]
      (function _ -> false)
      (l1,l2) in
  let rec loop = function
      ([],[]) -> []
    | (x::xs,[]) -> (compare_toplevel_lists x [])::(loop(xs,[]))
    | ([],y::ys) -> (compare_toplevel_lists [] y)::(loop([],ys))
    | (x::xs,y::ys) -> (compare_toplevel_lists x y)::(loop(xs,ys)) in
  List.concat
    (List.map2
       (function o1 -> function o2 -> loop (List.concat o1,List.concat o2))
       organized_l1 organized_l2)



(****************************************************************************)
(* Entry point *)
(****************************************************************************)

(* It would be nice to take care about doing unions in the right way, and
   the code above tries to do that.  But multiple ASTs print the same way, and
   ultimately, we only want to see each string once. *)
let diff t1 t2 =
  (*  Printf.printf "%s\n" (CE.ce2c(CE.CODELCE(t1,t2)));*)
  let compared = compare_toplevel_lists_lists t1 t2 in
  let res =
    List.fold_left
      (function rest ->
       function
         NO_CHANGE ->
         if !Config.notex then logger#info "no change\n\n"; rest
       | CHANGE(_,changelist) ->
         (*      let _ = assert (not (have_al_change_type changelist)) in *)

         let printed = ref [] in
         let print_change change n =
           let pchange = Ce_unparse.ce2c change in
           if not(List.mem pchange !printed)
           then
             begin
               logger#info "level %d: %s" n pchange;
               printed := pchange :: !printed
             end in	
         let rec loop n = function
             [] -> ()
           | CC(change,context)::rest
           | CG(change,context)::rest ->
             print_change change n; loop (n+1) context; loop n rest in
         let contextified = contextify changelist in
         (if !Config.notex then loop 0 contextified);
         contextified::rest)
      [] compared in
  List.rev res
