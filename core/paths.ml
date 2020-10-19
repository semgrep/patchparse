
(* can't be called path.ml because of possible linking conflict with 
 * compiler-libs/ when also use pfff libs and ppx *)

(* This used to be simple string aliases, but I've introduced a constructor
 * (a la Haskell newtype) to correctly identify all strings that 
 * were dirs or files.
 *)
type dir = Dir of string
 [@@deriving show { with_path = false }]
type file = File of string
 [@@deriving show { with_path = false }]
