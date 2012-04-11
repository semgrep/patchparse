type token =
  | IDENT of (string * (int * Parse_error.linetype))
  | CST_CHAR of (string * (int * Parse_error.linetype))
  | CST_INT of (string * (int * Parse_error.linetype))
  | CST_STRING of (string * (int * Parse_error.linetype))
  | SEP of (string * (int * Parse_error.linetype))
  | ESEP of (string * (int * Parse_error.linetype))
  | OPERATOR of (string * (int * Parse_error.linetype))
  | EQ of (string * (int * Parse_error.linetype))
  | SYMOP of (string * (int * Parse_error.linetype))
  | DEREFOP of (string * (int * Parse_error.linetype))
  | TYPE of (string * (int * Parse_error.linetype))
  | PRIM of (string * (int * Parse_error.linetype))
  | INCLUDE of (string * (int * Parse_error.linetype))
  | EOF
  | LPAREN of (int * Parse_error.linetype)
  | RPAREN of (int * Parse_error.linetype)
  | LBRACE of (int * Parse_error.linetype)
  | RBRACE of (int * Parse_error.linetype)
  | LBRACK of (int * Parse_error.linetype)
  | RBRACK of (int * Parse_error.linetype)
  | DEFINE of (int * Parse_error.linetype)

val interpret :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast0.code list
