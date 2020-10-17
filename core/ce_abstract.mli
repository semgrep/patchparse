open Ce

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

