
type info = line_number * Patch.linetype
  and line_number = int

type string_extended = string * info

type prim =
  | IDENT of string_extended

  | CHAR of string_extended
  | INT of string_extended
  | STR of string_extended
  | SYMOP of string_extended
  | TYPE of string_extended

  | ARRAY of expr list * Ast.known

and dprim = DEREFOP of string_extended

and symbol = prim list
and dsymbol = dprim list

and expr =
  | SYMBOL of symbol
  | DSYMBOL of dsymbol

  (* ??? *)
  | EOP of string_extended

  | ASSIGN of string_extended  * expr list * Ast.known
  | PAREN of code list * Ast.known
  | STRUCT of code list * Ast.known
  | CALL of string_extended * code list * Ast.known

  | DECLARER of string_extended * code list * Ast.known

and code =
  | EXPR of expr list
  | SEP of string_extended
