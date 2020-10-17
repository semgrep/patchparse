type context_change =
    CC of Ce.ce (* the change *) * context_change list
  | CG of Ce.ce (* a change generalized with EXP or CODE *) *
	context_change list

val diff : Ast.code list list -> Ast.code list list -> context_change list list

val al_context_change : context_change -> context_change

val have_al_context_change : context_change -> bool
