
(* can't be called path.ml because of possible linking conflict with 
 * compiler-libs/ when also use pfff libs and ppx *)

type dir = Dir of string
 [@@deriving show { with_path = false }]
type file = File of string
 [@@deriving show { with_path = false }]
