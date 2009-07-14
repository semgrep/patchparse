val real_fn : Ast.expr -> bool
val memory_mover : Ast.expr -> bool

(* ---------------------------------------------------------------------- *)

type ce = (* ce stands for "change element" *)
    PRIMCE of Ast.prim * Ast.prim
  | SYMCE of Ast.symbol * Ast.symbol
  | EXPRCE of Ast.expr * Ast.expr
  | EXPRLCE of Ast.expr list * Ast.expr list
  | CODECE of Ast.code * Ast.code
  | CODELCE of Ast.code list * Ast.code list

(* ---------------------------------------------------------------------- *)
(* abstract line functions *)

val al_ce : ce -> ce
val have_al_ce : ce -> bool

(* ---------------------------------------------------------------------- *)
(* printing functions *)

val ace2c : ce -> string
val ce2c : ce -> string
val ce2tex : ce -> string
val ce2parsable : ce -> string
val ce2c_simple : ce -> string
val ce2label : ce -> string
val clean : string -> string

(* ---------------------------------------------------------------------- *)
(* filtering functions *)

(* In some cases these are just boring, such as calls to dbg and err.  In
other cases, they do not contain enough information to determine whether
similar things are related or not, such as return, break, and continue. *)

val boring_ce : ce -> bool
val boring_prim : Ast.prim -> bool
val boring_symbol : Ast.symbol -> bool
val boring_expr : Ast.expr -> bool
val boring_expr_list : Ast.expr list -> bool
val boring_code : Ast.code -> bool
val boring_code_list : Ast.code list -> bool
      
(* ---------------------------------------------------------------------- *)
(* Abstraction *)

(* make a version of an expression with CODE replacing all function call
   arguments *)

val codify_expr_change : int ref (* gensym ctr *) -> ce -> ce
val codify_all_args : int ref (* gensym ctr *) -> ce -> ce
val mktail : int ref (* gensym ctr *) -> ce -> ce
      
(* ---------------------------------------------------------------------- *)
(* Abstraction, continued *)

(* Make a version of an expression with EXP replacing identifiers and some
expressions. *)

val expify : bool (* do_proto *) -> int ref (* gensym ctr *) -> ce -> ce

(* ---------------------------------------------------------------------- *)
(* get characters, for equivalence class creation.  result must be sorted *)

val get_characters : ce -> string list

(* ---------------------------------------------------------------------- *)
(* get the names of called functions *)

val get_function_names : ce -> string list * string list

(* ---------------------------------------------------------------------- *)
(* does a change involve a function call *)

val function_change : ce -> bool
