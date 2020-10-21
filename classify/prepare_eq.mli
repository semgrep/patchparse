(* just a wrapper over eqclasses to do and global analysis and special
   category analysis *)

val eqworklists :
  Change_tree.changelist -> unit

val eqclasses :
  bool (* whether the change table is needed after this step *) ->
  (Change_table.t (* global change table *) *
   Questions.result) *
  (string * Change_table.t (* category change table *) *
   Questions.result) list

(* internals *)
val gsemi__change_worklist: Change_table.worklist