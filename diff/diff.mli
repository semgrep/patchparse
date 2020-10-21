
val diff : Ast.code list list -> Ast.code list list -> 
  Change_tree.t list list

val al_context_change : Change_tree.t -> Change_tree.t

val have_al_context_change : Change_tree.t -> bool
