
(* pad: I've introduced those new types *)

(* patch number when processing a list of patches/commits *)
type id = Id of int 
 [@@deriving show { with_path = false }]


type line = 
  int (* line number in patch *) * 
  string (* patch line content *)
 [@@deriving show { with_path = false }]

type t = id * line list
 [@@deriving show { with_path = false }]

type region = 
  | Region of int
  | InFunction of string
 [@@deriving show { with_path = false }]

(* was in ast.ml *)
type linetype = 
  | PLUS 
  | MINUS 
  | CTX
 [@@deriving show { with_path = false }]
