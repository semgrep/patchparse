type line_number = int 
type info = line_number * Parse_error.linetype

type prim =
    IDENT of (string    *info)
  | CHAR of (string     *info)
  | INT of (string      *info)
  | STR of (string      *info)
  | SYMOP of (string    *info)
  | TYPE of (string     *info)
  | ARRAY of expr list * Ast.known

and dprim =
    DEREFOP of (string  *info)

and symbol = prim list
and dsymbol = dprim list

and expr =
    SYMBOL of symbol
  | DSYMBOL of dsymbol
  | EOP of (string    *info)
  | ASSIGN of (string *info)  * expr list * Ast.known
  | PAREN of code list * Ast.known
  | STRUCT of code list * Ast.known
  | CALL of (string *info) * code list * Ast.known
  | DECLARER of (string *info) * code list * Ast.known

and code =
    EXPR of expr list
  | SEP of (string     *info)


val convert : code list -> Ast.code list list
