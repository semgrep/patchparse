
(* patch number when processing a list of patches *)
type id = int 

type line = int (* line number? *) * string (* patch line content *)

type t = id * line list

type linetype = 
  | PLUS 
  | MINUS 
  | CTX