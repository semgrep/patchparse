
val patch : Common.filename -> Patch.t list

(* internals used by git_reader.ml *)
val lines : int ref
val counter: 'a list -> (int * 'a) list

(* returned parsed commit and rest of lines *)
val get_diffs: 
 (int * string) list -> (int * string) list * (int * string) list
