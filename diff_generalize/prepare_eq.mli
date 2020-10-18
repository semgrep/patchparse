open Eqclasses

(* just a wrapper over eqclasses to do and global analysis and special
category analysis *)

val eqworklists :
    Context_change.t list -> Context_change.origin -> unit

val eqclasses :
    bool (* whether the change table is needed after this step *) ->
      (change_table_type (* global change table *) *
         Questions.result) *
      (string * change_table_type (* category change table *) *
	 Questions.result) list
