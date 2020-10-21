
type changes = 
   (Patch.id * Paths.dir) * int (*sites*) *
    Paths.file list * Patch.region list *
    Change.ce list (* same as key but without abstraction lines*)
 [@@deriving show { with_path = false }]

type t = (Change.ce, changes list) Hashtbl.t

type workset = 
  (Change_tree.t * 
  Patch.id * Paths.dir * Paths.file * Patch.region) list ref
 [@@deriving show { with_path = false }]

(* the key is the size of the diff.ce of the context_change *)
type worklist = (int, workset) Hashtbl.t
