type token =
  | IDENT of (string * (int * Parse_error.linetype * Parse_error.atfront))
  | CST_CHAR of (string * (int * Parse_error.linetype * Parse_error.atfront))
  | CST_INT of (string * (int * Parse_error.linetype * Parse_error.atfront))
  | CST_STRING of (string * (int * Parse_error.linetype * Parse_error.atfront))
  | SEP of (string * (int * Parse_error.linetype * Parse_error.atfront))
  | ESEP of (string * (int * Parse_error.linetype * Parse_error.atfront))
  | OPERATOR of (string * (int * Parse_error.linetype * Parse_error.atfront))
  | EQ of (string * (int * Parse_error.linetype * Parse_error.atfront))
  | SYMOP of (string * (int * Parse_error.linetype * Parse_error.atfront))
  | DEREFOP of (string * (int * Parse_error.linetype * Parse_error.atfront))
  | TYPE of (string * (int * Parse_error.linetype * Parse_error.atfront))
  | PRIM of (string * (int * Parse_error.linetype * Parse_error.atfront))
  | INCLUDE of (string * (int * Parse_error.linetype * Parse_error.atfront))
  | EOF
  | LPAREN of (int * Parse_error.linetype * Parse_error.atfront)
  | RPAREN of (int * Parse_error.linetype * Parse_error.atfront)
  | LBRACE of (int * Parse_error.linetype * Parse_error.atfront)
  | RBRACE of (int * Parse_error.linetype * Parse_error.atfront)
  | LBRACK of (int * Parse_error.linetype * Parse_error.atfront)
  | RBRACK of (int * Parse_error.linetype * Parse_error.atfront)
  | DEFINE of (int * Parse_error.linetype * Parse_error.atfront)

val interpret :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast0.code list
