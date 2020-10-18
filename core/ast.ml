
(* ??? *)
type known = 
  | KNOWN 
  | ENDUNKNOWN 
  | FRONTUNKNOWN 
  | BOTHUNKNOWN

(* position information *)
type info = line_number * Patch.linetype
and line_number = int 

(* I also keep the 'a outside the closure cos I want to do pattern matching *)
type 'a extended = ('a * 'a kind)
and 'a kind = Abstract | Concrete of (unit -> ('a * info))

(* build extended *)
let bext (x,line) = x, Concrete (fun () -> (x,line))

(* extract *)
let eext e  = fst e
let lext = function
    Abstract -> failwith "abstract value, no line here"
  | Concrete f -> snd (f())


type prim =
  | IDENT of string extended

  | CHAR of  string extended
  | INT of   string extended
  | STR (*of string*)  of unit extended
  | SYMOP of string extended

  | ARRAY of expr list * known
  | PARENSYM of expr list * known

  (* ??? *)
  | EXP of int * prim option

and symbol = prim list

and expr =
  | SYMBOL of symbol

  (* ??? *)
  | EOP of string extended

  (* In the following, the first field is currently always a symbol.  But it 
     needs to change to allow function prototypes. *)
  | ASSIGN of expr * string extended * expr list * known

  (* In the following the first field is always a symbol.  Having symbol
     always as an expression is convenient for diff, because then we don't
     have to figure out how to turn a context change that might occur in
     the function part of a call (same for assignments) into a symbol so
     that it matches with other occurrences of the same change. *)
  | CALL of expr * code list * known

  | DECLARER of expr * code list * known
  | PROTOTYPE of expr (*name*) * symbol (*type*) *
                 string list (*static,init,inline,etc*) *
                 string (*name*) * code list * known

  | STRUCT of code list * known

and code =
  | EXPR of expr list
  | SEP of string extended
  (* ??? *)
  | ARG of int 
  (* ??? *)
  | CODE
