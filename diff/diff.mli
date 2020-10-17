
val diff : Ast.code list list -> Ast.code list list -> 
  Context_change.t list list

val al_context_change : Context_change.t -> Context_change.t

val have_al_context_change : Context_change.t -> bool
