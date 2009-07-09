type token =
  | IDENT of (string * int)
  | CST_CHAR of (string * int)
  | CST_INT of (string * int)
  | CST_STRING of (string * int)
  | SEP of (string * int)
  | ESEP of (string * int)
  | OPERATOR of (string * int)
  | EQ of (string * int)
  | SYMOP of (string * int)
  | DEREFOP of (string * int)
  | TYPE of (string * int)
  | PRIM of (string * int)
  | INCLUDE of (string * int)
  | EOF
  | LPAREN of (int)
  | RPAREN of (int)
  | LBRACE of (int)
  | RBRACE of (int)
  | LBRACK of (int)
  | RBRACK of (int)
  | DEFINE of (int)

val interpret :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast0.code list
