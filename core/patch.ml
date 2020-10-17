
type line = int (* patch line number *) * (int * string) list

type linetype = 
  | PLUS 
  | MINUS 
  | CTX
