
(* pad: I've introduced those new types *)

(* patch number when processing a list of patches/commits *)
type id = Id of int 

type line = 
  int (* line number in patch *) * 
  string (* patch line content *)

type t = id * line list

(* was in ast.ml *)
type linetype = 
  | PLUS 
  | MINUS 
  | CTX
