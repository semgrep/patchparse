type result =
    (string (*version*) *
       (string (*dir*) * (Ce.ce * int (*count*)) list) list) list *
      (string (*dir*) *
	 (string (*version*) * (Ce.ce * int (*count*)) list) list) list *
      (Ce.ce *
	 ((string (*version*) * string (*dir*)) * int (*count*)) list)
      list list *
      (Ce.ce *
	 ((string (*version*) * string (*dir*)) * int (*count*)) list) list *
      (Ce.ce *
	 ((string (*version*) * int (*count*)) * int (*ver#*)) list) list

(* the percentage of changes that are multiversion *)
val percentage_multiver : int ref
val total_changes : int ref

val questions :
    Eqclasses.change_table_type (* change table *) ->
      bool (* is the change table needed after this phase? *) ->
	bool (* compute percentage_multiver? not for filtered tables *) ->
	  result
