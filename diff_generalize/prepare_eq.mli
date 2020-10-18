(* just a wrapper over eqclasses to do and global analysis and special
category analysis *)

val eqworklists :
    Cc.t list -> Cc.origin -> unit

val eqclasses :
    bool (* whether the change table is needed after this step *) ->
      (Eq_classes.change_table_type (* global change table *) *
         Questions.result) *
      (string * Eq_classes.change_table_type (* category change table *) *
	 Questions.result) list
