
type change_table =
    (Ce.ce,
     ((Patch.id (*version*) * Paths.dir (*dir*)) * int (*sites*) *
	Paths.file list (*files*) * Patch.region list (*regions*) *
	Ce.ce list (* same as key but without abstraction lines*)) list
    ) Hashtbl.t

(* the key is the size of the diff.ce of the context_change *)
type worklist = 
    (int, (Context_change.t * Patch.id (*version*) * Paths.dir (*dir*) *
	     Paths.file (*file*) * Patch.region (*region*)) list ref)
      Hashtbl.t
