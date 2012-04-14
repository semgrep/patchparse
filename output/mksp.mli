val make_files :
    (Questions.result *
       (string * Eqclasses.change_table_type (* change table *) *
	  Questions.result) list) ->
	    (int(*ver*) * string(*dir*) *
	       string list(*files*) * Ce.ce list * float (*weight*)) list
		 -> unit
