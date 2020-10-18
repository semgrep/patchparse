
val eqworklists :
    Eq_classes.worklist (* worklist, inout, !modified! *) ->
    int ref (* max size storage *) ->
    Cc.t -> Cc.origin ->
    unit

val eqclasses :
  Eq_classes.worklist (* worklist, in *) ->
    Eq_classes.change_table (* change table, inout, !modified! *) ->
      int ref (* max size storage *)  -> unit

val print_change_table : Eq_classes.change_table -> unit

val for_debug : Eq_classes.worklist -> unit

val version_unused_table : (* unused tokens in each version *)
    (string (* version *), int ref (* unused tokens *)) Hashtbl.t
