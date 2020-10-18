val collect :
    Eq_classes.change_table ->
      (Patch.id(*ver*) * string(*dir*) *
	 string list(*files*) * Ce.ce list * float) list
