(* filtering functions *)

(* In some cases these are just boring, such as calls to dbg and err.  In
   other cases, they do not contain enough information to determine whether
   similar things are related or not, such as return, break, and continue. *)

val boring_ce : Change.ce -> bool

val boring_prim : Ast.prim -> bool
val boring_symbol : Ast.symbol -> bool
val boring_expr : Ast.expr -> bool
val boring_expr_list : Ast.expr list -> bool
val boring_code : Ast.code -> bool
val boring_code_list : Ast.code list -> bool
