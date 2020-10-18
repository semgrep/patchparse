
val eqworklists :
    Eq_classes.worklist_type (* worklist, inout, !modified! *) ->
      int ref (* max size storage *) ->
	Cc.t ->
	  (Patch.id * string (*path*) * string (*file*) * string (*region*) ) 
	  -> unit

val eqclasses :
  Eq_classes.worklist_type (* worklist, in *) ->
    Eq_classes.change_table_type (* change table, inout, !modified! *) ->
      int ref (* max size storage *)  -> unit

val print_change_table : Eq_classes.change_table_type -> unit

val for_debug : Eq_classes.worklist_type -> unit

val version_unused_table : (* unused tokens in each version *)
    (string (* version *), int ref (* unused tokens *)) Hashtbl.t
