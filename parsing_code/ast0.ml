
type string_extended = string * Ast.info
 [@@deriving show { with_path = false }]

type prim =
  | IDENT of string_extended

  | CHAR of string_extended
  | INT of string_extended
  | STR of string_extended
  | SYMOP of string_extended

  (* ??? not in Ast.prim *)
  | TYPE of string_extended

  | ARRAY of expr list * Ast.known

(* pad: a pointer derefence or a multiplication *)
and dprim = DEREFOP of string_extended

(* list? *)
and symbol = prim list
(* d?? *)
and dsymbol = dprim list

and expr =
  | SYMBOL of symbol
  | DSYMBOL of dsymbol

  (* ??? *)
  | EOP of string_extended

  | ASSIGN of string_extended  * expr list * Ast.known
  | CALL of string_extended * code list * Ast.known

  | DECLARER of string_extended * code list * Ast.known

  | PAREN of code list * Ast.known
  | STRUCT of code list * Ast.known


and code =
  | EXPR of expr list
  (* , ; { } *)
  | SEP of string_extended

and codelist = code list

 [@@deriving show { with_path = false }]
