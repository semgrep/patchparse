
(* ce stands for "change element" *)
type t =
    PRIMCE of Ast.prim * Ast.prim
  | SYMCE of Ast.symbol * Ast.symbol
  | EXPRCE of Ast.expr * Ast.expr
  | EXPRLCE of Ast.expr list * Ast.expr list
  | CODECE of Ast.code * Ast.code
  | CODELCE of Ast.code list * Ast.code list
 [@@deriving show { with_path = false }]

type ce = t
 [@@deriving show { with_path = false }]

