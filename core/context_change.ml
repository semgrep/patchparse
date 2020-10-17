
(* used to be in diff.ml *)
type t =
  | CC of Ce.ce (* the change *) * t list
  | CG of Ce.ce (* a change generalized with EXP or CODE *) * t list
