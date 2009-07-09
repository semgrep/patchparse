type line_number = int 

type prim =
    IDENT of (string    *line_number)
  | CHAR of (string     *line_number)
  | INT of (string      *line_number)
  | STR of (string      *line_number)
  | SYMOP of (string    *line_number)
  | TYPE of (string     *line_number)
  | ARRAY of expr list * Ast.known

and dprim =
    DEREFOP of (string  *line_number)

and symbol = prim list
and dsymbol = dprim list

and expr =
    SYMBOL of symbol
  | DSYMBOL of dsymbol
  | EOP of (string    *line_number)
  | ASSIGN of (string *line_number)  * expr list * Ast.known
  | PAREN of code list * Ast.known
  | STRUCT of code list * Ast.known
  | CALL of (string *line_number) * code list * Ast.known

and code =
    EXPR of expr list
  | SEP of (string     *line_number)


val convert : code list -> Ast.code list list
