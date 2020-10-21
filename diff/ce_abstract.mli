
(* make a version of an expression with CODE replacing all function call
   arguments *)

val codify_expr_change : int ref (* gensym ctr *) -> Change.ce -> Change.ce
val codify_all_args : int ref (* gensym ctr *) -> Change.ce -> Change.ce
val mktail : int ref (* gensym ctr *) -> Change.ce -> Change.ce

(* Make a version of an expression with EXP replacing identifiers and some
   expressions. *)

val expify : bool (* do_proto *) -> int ref (* gensym ctr *) -> Change.ce ->
  Change.ce

