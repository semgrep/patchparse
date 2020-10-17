type change_table_type =
    (Ce.ce,
     ((int (*version*) * string (*dir*)) * int (*sites*) *
	string list (*files*) * string list (*regions*) *
	Ce.ce list (* same as key but without abstraction lines*)) list
    ) Hashtbl.t

(* the key is the size of the diff.ce of the context_change *)
type worklist_type = 
    (int, (Context_change.t * int (*version*) * string (*dir*) *
	     string (*file*) * string (*region*)) list ref)
      Hashtbl.t

val eqworklists :
    worklist_type (* worklist, inout, !modified! *) ->
      int ref (* max size storage *) ->
	Context_change.t ->
	  (int * string (*path*) * string (*file*) * string (*region*) ) 
	  -> unit

val eqclasses :
  worklist_type (* worklist, in *) ->
    change_table_type (* change table, inout, !modified! *) ->
      int ref (* max size storage *)  -> unit

val print_change_table : change_table_type -> unit

val for_debug : worklist_type -> unit

val version_unused_table : (* unused tokens in each version *)
    (string (* version *), int ref (* unused tokens *)) Hashtbl.t
