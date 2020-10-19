(* cc stands for context change *)

(* was in diff.ml *)
type t =
  | CC of Ce.ce (* the change *) * t list
  | CG of Ce.ce (* a change generalized with EXP or CODE *) * t list
 [@@deriving show { with_path = false }]

(* pad: I've introduced those new types *)

type origin = 
  Patch.id * Paths.dir (*path*) * Paths.file (*file*) * Patch.region
 [@@deriving show { with_path = false }]

type changelist = t list * origin
 [@@deriving show { with_path = false }]

