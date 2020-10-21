
(* position information *)
type info = line_number * Patch.linetype
  and line_number = int (* line number in the patch, not in original code *)
 [@@deriving show { with_path = false }]

(* I also keep the 'a outside the closure cos I want to do pattern matching *)
type 'a kind = Abstract | Concrete of (unit -> ('a * info))
let pp_kind _x fmt x = 
  Format.fprintf fmt "()"

type 'a extended = ('a * 'a kind)
 [@@deriving show { with_path = false }]

(* build extended *)
let bext (x,line) = x, Concrete (fun () -> (x,line))

(* extract *)
let eext e  = fst e
let lext = function
    Abstract -> failwith "abstract value, no line here"
  | Concrete f -> snd (f())


(* pad: used for a sequence of things (e.g., arguments), to record whether
 * we parsed from start to end the sequence or whether some parts were unknown.
 *)
type known = 
  | KNOWN 
  | ENDUNKNOWN 
  | FRONTUNKNOWN 
  | BOTHUNKNOWN
 [@@deriving show { with_path = false }]


type prim =
  | IDENT of string extended

  | CHAR of  string extended
  | INT of   string extended
  | STR (*of string*)  of unit extended
  | SYMOP of string extended

  | ARRAY of expr list * known

  (* ??? not in Ast0.prim, was in Ast0.expr instead *)
  | PARENSYM of expr list * known

  (* pad: Generalized prim, with optional original prim *)
  | EXP of int * prim option

(* list? *)
and symbol = prim list

and expr =
  | SYMBOL of symbol

  (* ??? Expression Operator? *)
  | EOP of string extended

  (* In the following, the first field is currently always a symbol.  But it 
     needs to change to allow function prototypes. *)
  | ASSIGN of expr * string  extended (* = or -=, +=, ...*) * expr list * known

  (* In the following the first field is always a symbol.  Having symbol
     always as an expression is convenient for diff, because then we don't
     have to figure out how to turn a context change that might occur in
     the function part of a call (same for assignments) into a symbol so
     that it matches with other occurrences of the same change. *)
  (* pad: note that even if(...) are parsed as a Call *)
  | CALL of expr * code list * known

  | DECLARER of expr * code list * known
  (* ?? not in Ast0.expr *)
  | PROTOTYPE of expr (*name*) * symbol (*type*) *
                 string list (*static,init,inline,etc*) *
                 string (*name*) * code list * known

  | STRUCT of code list * known

and code =
  | EXPR of expr list
  | SEP of string extended

  (* pad: Generalized argument (not in Ast0). The int is the argument
   * position in the original code *)
  | ARG of int 
  (* pad: Even more generalized argument (also not in Ast0), we don't even
   * record the original position.
   *)
  | CODE

and codelist = code list
 [@@deriving show { with_path = false }]
