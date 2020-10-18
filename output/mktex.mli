val make_files :
    (Questions.result *
       (string * Eq_classes.change_table_type (* change table *) *
	  Questions.result) list) ->
	    (Patch.id(*ver*) * string(*dir*) *
	       string list(*files*) * Ce.ce list * float (*weight*)) list
		 -> unit
