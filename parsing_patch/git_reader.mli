
(* file can be a gitcommitlist for example *)
val git : Common.filename   -> Patch.t list

val patch : Common.filename -> Patch.t list
