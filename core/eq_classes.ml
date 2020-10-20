
type changes = 
   (Patch.id * Paths.dir) * int (*sites*) *
    Paths.file list * Patch.region list *
    Ce.ce list (* same as key but without abstraction lines*)
 [@@deriving show { with_path = false }]

type change_table = (Ce.ce, changes list) Hashtbl.t

type workset = 
  (Context_change.t * 
  Patch.id * Paths.dir * Paths.file * Patch.region) list ref
 [@@deriving show { with_path = false }]

(* the key is the size of the diff.ce of the context_change *)
type worklist = (int, workset) Hashtbl.t
