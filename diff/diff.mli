
val diff : Ast.code list list -> Ast.code list list -> 
  Cc.t list list

val al_context_change : Cc.t -> Cc.t

val have_al_context_change : Cc.t -> bool
