
val eqworklists :
  Change_table.worklist (* worklist, inout, !modified! *) ->
  int ref (* max size storage *) ->
  Change_tree.t -> Change_tree.origin ->
  unit

val eqclasses :
  Change_table.worklist (* worklist, in *) ->
  Change_table.t (* change table, inout, !modified! *) ->
  int ref (* max size storage *)  -> unit

val print_change_table : Change_table.t -> unit

val version_unused_table : (* unused tokens in each version *)
  (string (* version *), int ref (* unused tokens *)) Hashtbl.t
