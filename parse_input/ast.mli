type known = KNOWN | ENDUNKNOWN | FRONTUNKNOWN | BOTHUNKNOWN


type line_number = int 
type linetype = Parse_error.linetype
type info = line_number * linetype
type 'a extended = ('a * 'a kind) (* I also keep the 'a outside the closure cos I want to do pattern matching *)
 and 'a kind = Abstract | Concrete of (unit -> ('a * info))

(* build extended *)
val bext: ('a * info) -> 'a extended

(* extract *)
val eext: 'a extended  -> 'a
val lext: 'a kind -> info

type prim =
    IDENT of string extended
  | CHAR of  string extended
  | INT of   string extended
  | STR (*of string*)  of unit extended
  | SYMOP of string extended
  | ARRAY of expr list * known
  | PARENSYM of expr list * known
  | EXP of int * prim option

and symbol = prim list

and expr =
    SYMBOL of symbol
  | EOP of string extended
  (* In the following, the first field is currently always a symbol.  But it 
     needs to change to allow function prototypes. *)
  | ASSIGN of expr * string extended * expr list * known
  (* In the following the first field is always a symbol.  Having symbol
     always as an expression is convenient for diff, because then we don't
     have to figure out how to turn a context change that might occur in
     the function part of a call (same for assignments) into a symbol so
     that it matches with other occurrences of the same change. *)
  | CALL of expr * code list * known
  | PROTOTYPE of expr (*name*) * symbol (*type*) *
	string list (*static,init,inline,etc*) *
	string (*name*) * code list * known
  | STRUCT of code list * known

and code =
    EXPR of expr list
  | SEP of string extended
  | ARG of int 
  | CODE



val ast_unparse_prim : prim -> string
val ast_unparse_symbol : prim list -> string
val ast_unparse_symbol_list : prim list list -> string
val ast_unparse_expr : expr -> string
val ast_unparse_expr_list : expr list -> string
val ast_unparse_code : code -> string
val ast_unparse_code_list : code list -> string

val unparse_prim : prim -> string
val unparse_symbol : prim list -> string
val unparse_symbol_list : prim list list -> string
val unparse_expr : expr -> string
val unparse_expr_list : expr list -> string
val unparse_code : code -> string
val unparse_code_list : code list -> string

val unparse : code list -> string


val unparse_sp_prim : bool -> prim -> string
val unparse_sp_symbol : bool -> prim list -> string
val unparse_sp_symbol_list : bool -> prim list list -> string
val unparse_sp_expr : bool -> expr -> string
val unparse_sp_expr_list : bool -> expr list -> string
val unparse_sp_code : bool -> code -> string
val unparse_sp_code_list : bool -> code list -> string

val unparse_minus : (bool -> 'a -> string) -> 'a -> string
val unparse_plus :
    (bool -> 'a -> string) -> 'a -> (string list (* metavars *) * string)


val al_prim:   prim -> prim
val al_symbol: symbol -> symbol
val al_expr:   expr -> expr
val al_code:   code -> code

val have_al_prim:   prim -> bool
val have_al_symbol: symbol -> bool
val have_al_expr:   expr -> bool
val have_al_code:   code -> bool
