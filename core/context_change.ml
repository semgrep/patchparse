(* cc stands for context change *)

(* was in diff.ml *)
type t =
  | CC of Ce.ce (* the change *) * t list
  | CG of Ce.ce (* a change generalized with EXP or CODE *) * t list

(* pad: I've introduced those new types *)

type origin = 
  Patch.id * Paths.dir (*path*) * Paths.file (*file*) * Patch.region

type changelist = t list * origin

