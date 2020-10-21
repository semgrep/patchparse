(* just a wrapper over eqclasses to do and global analysis and special
   category analysis *)

val eqworklists :
  Change_tree.changelist -> unit

val eqclasses :
  bool (* whether the change table is needed after this step *) ->
  (Eq_classes.change_table (* global change table *) *
   Questions.result) *
  (string * Eq_classes.change_table (* category change table *) *
   Questions.result) list

(* internals *)
val gsemi__change_worklist: Eq_classes.worklist