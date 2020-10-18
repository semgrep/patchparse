
type change_table =
    (Ce.ce,
     ((Patch.id (*version*) * string (*dir*)) * int (*sites*) *
	string list (*files*) * string list (*regions*) *
	Ce.ce list (* same as key but without abstraction lines*)) list
    ) Hashtbl.t

(* the key is the size of the diff.ce of the context_change *)
type worklist = 
    (int, (Cc.t * Patch.id (*version*) * string (*dir*) *
	     string (*file*) * string (*region*)) list ref)
      Hashtbl.t
