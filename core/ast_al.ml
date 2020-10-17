open Ast

let is_al e = match e with Abstract -> true | _ -> false

(* ---------------------------- abstract line --------------------------- *)

let _al_number = Abstract
let rec al_prim = function
  | IDENT (string, line_number)    -> IDENT (string, _al_number)
  | CHAR  (string, line_number)    -> CHAR  (string, _al_number)
  | INT   (string, line_number)    -> INT   (string, _al_number)
  | STR (*of string*) line_number  -> STR (*of string*) ((), _al_number)
  | SYMOP  (string,   line_number) -> SYMOP  (string,   _al_number)
  | ARRAY  (expr_list, known)      ->
      ARRAY  (List.map al_expr expr_list, known)
  | PARENSYM  (expr_list, known)   ->
      PARENSYM  (List.map al_expr expr_list, known)
  | EXP(int,exp_opt)                       -> EXP(int,None)

and al_symbol = function prim_list -> List.map al_prim prim_list

and al_expr = function
  | SYMBOL  symbol -> SYMBOL  (al_symbol symbol)
  | EOP (string,  line_number) -> EOP (string,  _al_number)
  (* In the following, the first field is currently always a symbol.  But it 
     needs to change to allow function prototypes. *)
  | ASSIGN (expr , (string, line_number) , expr_list , known) -> 
      ASSIGN (al_expr expr, (string, _al_number),
	      List.map al_expr expr_list, known)
  (* In the following the first field is always a symbol.  Having symbol
     always as an expression is convenient for diff, because then we don't
     have to figure out how to turn a context change that might occur in
     the function part of a call (same for assignments) into a symbol so
     that it matches with other occurrences of the same change. *)
  | CALL (expr, code_list, known) ->
      CALL (al_expr expr, List.map al_code code_list, known)
  | DECLARER (expr, code_list, known) ->
      DECLARER (al_expr expr, List.map al_code code_list, known)
  | PROTOTYPE (expr (*name*) , symbol (*type*) ,
	string_list (*static,init,inline,etc*) ,
	string (*name*) , code_list , known)  -> 
    PROTOTYPE (al_expr expr (*name*) , al_symbol symbol (*type*) ,
	string_list (*static,init,inline,etc*) ,
	string (*name*) ,List.map al_code code_list, known) 
  | STRUCT (code_list , known) ->
      STRUCT (List.map al_code code_list, known)

and al_code = function
  | EXPR  expr_list -> EXPR (List.map al_expr expr_list)
  | SEP  (string , line_number) -> SEP (string, _al_number)
  | ARG  int  -> ARG int
  | CODE -> CODE



(* ---------------------------- have_al --------------------------- *)

let rec have_al_prim = function
  | IDENT (string, line_number)    -> is_al line_number
  | CHAR  (string, line_number)    -> is_al line_number
  | INT   (string, line_number)    -> is_al line_number
  | STR (*of string*) ((),line_number)  -> is_al line_number
  | SYMOP  (string,   line_number) -> is_al line_number
  | ARRAY  (expr_list, known)      -> List.exists have_al_expr expr_list
  | PARENSYM  (expr_list, known)   -> List.exists have_al_expr expr_list
  | EXP  (int,_)                   -> false

and have_al_symbol =
  function prim_list -> List.exists have_al_prim prim_list

and have_al_expr = function
  | SYMBOL  symbol ->  (have_al_symbol symbol)
  | EOP (string,  line_number) -> is_al line_number
  | ASSIGN (expr , (string, line_number) , expr_list , known) -> 
       (have_al_expr expr || (is_al line_number) ||
       (List.exists have_al_expr expr_list))
  | CALL (expr, code_list, known) | DECLARER (expr, code_list, known) ->
      (have_al_expr expr || (List.exists have_al_code code_list))
  | PROTOTYPE (expr (*name*) , symbol (*type*) ,
	string_list (*static,init,inline,etc*) ,
	string (*name*) , code_list , known)  -> 
    (have_al_expr expr (*name*) || have_al_symbol symbol (*type*) ||
	(List.exists have_al_code code_list) )
  | STRUCT (code_list , known) -> List.exists have_al_code code_list

and have_al_code = function
  | EXPR  expr_list -> List.exists have_al_expr expr_list
  | SEP  (string , line_number) -> is_al line_number
  | ARG  int  -> false
  | CODE -> false

