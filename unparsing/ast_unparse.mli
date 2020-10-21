open Ast

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
  (bool -> 'a -> string) -> 'a ->
  (string list (* raw metavars *) * string list (* metavars *) *
   bool (*invalid*) * string)

