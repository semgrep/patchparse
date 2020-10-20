
(* pad: I've introduced all those new types *)

(* patch number when processing a list of patches/commits *)
type id = Id of int 
 [@@deriving show { with_path = false }]

type line = 
  int (* line number in patch *) * 
  string (* patch line content *)
 [@@deriving show { with_path = false }]

(* unparsed patch *)
type t = id * line list
 [@@deriving show { with_path = false }]

(* Leveraging the region heuristic of diff, for example on
 * '@@ 1,1 @@ void foo()' 
 * we will store InFunction "foo".
 *)
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

(* parsed patch *)
type hunk = {
    patch_id: id;

    dir: Paths.dir;
    file: Paths.file;
    start_line: int;
    region: region;

    (* minus and context part *)
    minus: string;
    (* plus and context part *)
    plus: string;
}
 [@@deriving show { with_path = false }]
