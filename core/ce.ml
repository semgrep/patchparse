
type ce = (* ce stands for "change element" *)
    PRIMCE of Ast.prim * Ast.prim
  | SYMCE of Ast.symbol * Ast.symbol
  | EXPRCE of Ast.expr * Ast.expr
  | EXPRLCE of Ast.expr list * Ast.expr list
  | CODECE of Ast.code * Ast.code
  | CODELCE of Ast.code list * Ast.code list
