(* ---------------------------------------------------------------------- *)
(* get characters, for equivalence class creation.  result must be sorted *)

val get_characters : Change.ce -> string list

(* ---------------------------------------------------------------------- *)
(* get the names of called functions *)

val get_function_names : Change.ce -> string list * string list

(* ---------------------------------------------------------------------- *)
(* does a change involve a function call *)

val function_change : Change.ce -> bool
