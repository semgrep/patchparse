open Ce
(* ---------------------------------------------------------------------- *)
(* get characters, for equivalence class creation.  result must be sorted *)

val get_characters : ce -> string list

(* ---------------------------------------------------------------------- *)
(* get the names of called functions *)

val get_function_names : ce -> string list * string list

(* ---------------------------------------------------------------------- *)
(* does a change involve a function call *)

val function_change : ce -> bool
