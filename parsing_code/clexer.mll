{
open Cparser2
open Parse_error

exception Eof
exception InternalError of string

let version =
  "Clexer V1.0f 10.8.99 Hugues Cassé"^"\nmany changes by Julia Lawall"

(*****************************************************************************)
(* Rule token *)
(*****************************************************************************)


let _current_line = ref 0
let set_current_line i = _current_line := i
let current_line () = !_current_line

let linetype = ref Patch.CTX
let atfront = ref FRONT

let lexeme_line lexbuf =
  let af = !atfront in
  atfront := AFTER;
  (Lexing.lexeme lexbuf, (!_current_line, !linetype,af))

(*
** Keyword hashtable
*)

(*
** Useful primitives
*)
let rem_quotes str = String.sub str 1 ((String.length str) - 2)
let scan_ident id =
  match List.rev (Str.split (Str.regexp "_") (fst id)) with
    "t"::_|"T"::_ -> TYPE id (* typically typedef, at least for Linux *)
  | _ -> IDENT id
(*
** Buffer processor
*)

let set_line num =
    let (inter, lin, buf, pos, _, out, name) = !current_handle in
    current_handle := (inter, lin, buf, pos, num - 1, out, name)

let set_name name =
    let (inter, lin, buf, pos, num, out, _) = !current_handle in
    current_handle := (inter, lin, buf, pos, num, out, name)

    
(*** Error handling ***)
let error msg =
  display_error msg (Parsing.symbol_start ()) (Parsing.symbol_end ());
  raise Parsing.Parse_error
    

(*** escape character management ***)
let scan_escape str =
  match str with
    "n" -> "\n"
  | "r" -> "\r"
  | "t" -> "\t"
  | "b" -> "\b"
  | _ -> str
let get_value chr =
  match chr with
    '0'..'9' -> (Char.code chr) - (Char.code '0')
  | 'a'..'z' -> (Char.code chr) - (Char.code 'a') + 10
  | 'A'..'Z' -> (Char.code chr) - (Char.code 'A') + 10
  | _ -> 0
let scan_hex_escape str =
  String.make 1 (Char.chr (
         (get_value (String.get str 0)) * 16
           + (get_value (String.get str 1))
           ))
let scan_oct_escape str =
  String.make 1 (Char.chr (
         (get_value (String.get str 0)) * 64
           + (get_value (String.get str 1)) * 8
           + (get_value (String.get str 2))
           ))
}

(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)

let decdigit = ['0'-'9']
let octdigit = ['0'-'7']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let letter = ['a'- 'z' 'A'-'Z' 'æ' 'ø' 'å' 'é' 'ä' '©' '`' '´' '$']

let usuffix = ['u' 'U']
let lsuffix = ['l' 'L']
let intsuffix = (lsuffix|usuffix|(usuffix lsuffix)|(lsuffix usuffix))?
let floatsuffix = ['f' 'F' 'l' 'L']

let intnum = decdigit+ intsuffix?
let octnum = '0' octdigit+ intsuffix?
let hexnum = '0' ['x' 'X'] hexdigit+ intsuffix?

let exponent = ['e' 'E']['+' '-']? decdigit+
let fraction  = '.' decdigit+
let floatraw = (intnum? fraction)
            |(intnum exponent)
            |(intnum? fraction exponent)
            |(intnum '.') 
let floatnum = floatraw floatsuffix?

let ident =
  (letter|'_'|'@'|'#')(letter|decdigit|'_'|'@'|'\'')* 
let blank = [' ' '\t' '\n' '\\']
let escape = '\\' _
let hex_escape = '\\' ['x' 'X'] hexdigit hexdigit
let oct_escape = '\\' octdigit  octdigit octdigit

(*****************************************************************************)
(* Rule initial *)
(*****************************************************************************)

rule initial =
    parse   "/*"        {let _ = comment lexbuf in initial lexbuf}
(*        |  '\n'                 { incr _current_line; initial lexbuf } *)
        | "___line=" (['0' - '9']+ as line_num)
        { set_current_line (int_of_string line_num); initial lexbuf }
    |       blank       {initial lexbuf}
    |       "*/"        {initial lexbuf}
    |       "//"        {endline lexbuf}
    |       '\012'      {initial lexbuf} (* ^L *)
(*  |       '#'     {line lexbuf}*)
    
    |'"' {let cur = current_line() in
          CST_STRING (str lexbuf, (cur, !linetype,!atfront))(*ENOUGH ?*)} 
    |   floatnum    {CST_INT (lexeme_line lexbuf)}
    |   hexnum      {CST_INT (lexeme_line lexbuf)}
    |   octnum      {CST_INT (lexeme_line lexbuf)}
    |   intnum      {CST_INT (lexeme_line lexbuf)}

    |   "!quit!"    {EOF}

    |   "..."       {OPERATOR(lexeme_line lexbuf)}

    |   "+="        {EQ(lexeme_line lexbuf)}
    |   "-="        {EQ(lexeme_line lexbuf)}
    |   "*="        {EQ(lexeme_line lexbuf)}
    |   "/="        {EQ(lexeme_line lexbuf)}
    |   "%="        {EQ(lexeme_line lexbuf)}
    |   "|="        {EQ(lexeme_line lexbuf)}
    |   "&="        {EQ(lexeme_line lexbuf)}
    |   "^="        {EQ(lexeme_line lexbuf)}
    |   "<<="       {EQ(lexeme_line lexbuf)}
    |   ">>="       {EQ(lexeme_line lexbuf)}

    |   "<<"        {OPERATOR(lexeme_line lexbuf)}
    |   ">>"        {OPERATOR(lexeme_line lexbuf)}
    |   "=="        {OPERATOR(lexeme_line lexbuf)}
    |   "!="        {OPERATOR(lexeme_line lexbuf)}
    |   "<="        {OPERATOR(lexeme_line lexbuf)}
    |   ">="        {OPERATOR(lexeme_line lexbuf)}
    |   "="     {EQ(lexeme_line lexbuf)}
    |   "<"     {OPERATOR(lexeme_line lexbuf)}
    |   ">"     {OPERATOR(lexeme_line lexbuf)}
    |   "++"        {SYMOP(lexeme_line lexbuf)}
    |   "--"        {SYMOP(lexeme_line lexbuf)}
    |   "->"        {SYMOP(lexeme_line lexbuf)}
    |   '+'     {OPERATOR(lexeme_line lexbuf)}
    |   '-'     {OPERATOR(lexeme_line lexbuf)}

    |   '*'     {DEREFOP(lexeme_line lexbuf)}

    |   '/'     {OPERATOR(lexeme_line lexbuf)}
    |   '%'     {OPERATOR(lexeme_line lexbuf)}
    |   '!'     {OPERATOR(lexeme_line lexbuf)}
    |   "&&"        {OPERATOR(lexeme_line lexbuf)}
    |   "||"        {OPERATOR(lexeme_line lexbuf)}
    |   '&'     {DEREFOP(lexeme_line lexbuf)}
    |   '|'     {OPERATOR(lexeme_line lexbuf)}
    |   '^'     {OPERATOR(lexeme_line lexbuf)}
    |   '?'     {OPERATOR(lexeme_line lexbuf)}
    |   ':'     {OPERATOR(lexeme_line lexbuf)}
    |   '~'     {OPERATOR(lexeme_line lexbuf)}

    |       '{'     {LBRACE (current_line(),!linetype,!atfront)}
    |       '}'     {RBRACE (current_line(),!linetype,!atfront)}
    |       '['     {LBRACK (current_line(),!linetype,!atfront)}
    |       ']'     {RBRACK (current_line(),!linetype,!atfront)}
    |       '('     {LPAREN (current_line(),!linetype,!atfront)}
    |       ')'     {RPAREN (current_line(),!linetype,!atfront)}
    |               "#define"       {endline lexbuf}
    |               "#ifdef"        {endline lexbuf}
    |               "#ifndef"       {endline lexbuf}
    |               "#if"           {endline lexbuf}
    |               "#else"         {endline lexbuf}
    |               "#endif"        {endline lexbuf}
    |               "#error"        {endline lexbuf}
    |               "#warning"      {endline lexbuf}
    |               "#include"      
        {let (x,info) = lexeme_line lexbuf in
        INCLUDE (String.concat "" (x::(idline lexbuf)),info)}
    |               "if"            {PRIM(lexeme_line lexbuf)}
    |               "while"         {PRIM(lexeme_line lexbuf)}
    |               "for"           {PRIM(lexeme_line lexbuf)}
    |               "switch"        {PRIM(lexeme_line lexbuf)}
    |       ';'     {ESEP(lexeme_line lexbuf)}
    |       ','     {ESEP(lexeme_line lexbuf)}
    |       '.'     {SYMOP(lexeme_line lexbuf)}
    |               "int"           {TYPE(lexeme_line lexbuf)}
    |               "long"          {TYPE(lexeme_line lexbuf)}
    |               "uint"          {TYPE(lexeme_line lexbuf)}
    |               "ulong"         {TYPE(lexeme_line lexbuf)}
    |               "void"          {TYPE(lexeme_line lexbuf)}
    |               "unsigned"      {TYPE(lexeme_line lexbuf)}
    |               "u8"|"u16"|"u32"|"u64" {TYPE(lexeme_line lexbuf)}
    |               "s8"|"s16"|"s32"|"s64" {TYPE(lexeme_line lexbuf)}
    |               "struct"        {TYPE(lexeme_line lexbuf)}
    | ident '<' ident '>'       {scan_ident (lexeme_line lexbuf)}
    | ident '<' '>'         {scan_ident (lexeme_line lexbuf)}
    |       ident       {scan_ident (lexeme_line lexbuf)}
    | "++++plus_line++++"
        { linetype := PLUS; atfront := FRONT; initial lexbuf }
    | "++++minus_line++++"
        { linetype := MINUS; atfront := FRONT; initial lexbuf }
    | "++++context_line++++"
        { linetype := CTX; atfront := FRONT; initial lexbuf }
    | "++++space++++"
        { atfront := AFTER; initial lexbuf }
    | '\''  {let cur = current_line() in
             CST_CHAR (chr lexbuf, (cur,!linetype,!atfront)) (*ENOUGH?*)}
    
    |       eof     {EOF}
    |       _       {display_error
                        ("Invalid symbol: "^
                         (Lexing.lexeme lexbuf))
                        (Lexing.lexeme_start lexbuf)
                        (Lexing.lexeme_end lexbuf);
                            initial lexbuf}
and comment =
    parse   "*/"            {()}
    | eof                           {()}
    |       _       {comment lexbuf}

(* # <line number> <file name> ... *)
and line =
    parse   '\n'            {initial lexbuf}
    | blank             {line lexbuf}
    | intnum {set_line (int_of_string (Lexing.lexeme lexbuf));
                      file lexbuf}
    |   _           {endline lexbuf}
and file =
    parse '\n'          {initial lexbuf}
    |   blank           {file lexbuf}
    |   '"' [^ '"']* '"'    {set_name (rem_quotes (Lexing.lexeme lexbuf));
                      endline lexbuf}
    |   _           {endline lexbuf}
and endline_slash =
    parse '\n'          {endline lexbuf}
    |     eof           {initial lexbuf}
    |   _           {endline_slash lexbuf}
and endline =
    parse '\n'          {initial lexbuf}
    |     eof           {initial lexbuf}
    |   '\\'            {endline_slash lexbuf}
    |   _           {endline lexbuf}

and idline =
    parse '\n'          {[]}
    |   _           {let x = Lexing.lexeme lexbuf in x::idline lexbuf}

and str =
    parse   '"'         {""}
        | eof               {""}
    |       hex_escape  {let cur = scan_hex_escape (String.sub
                                      (Lexing.lexeme lexbuf) 2 2) in cur ^ (str lexbuf)}
    |       oct_escape  {let cur = scan_oct_escape (String.sub
                                      (Lexing.lexeme lexbuf) 1 3) in cur ^ (str lexbuf)}
    |       "\\0"       {(String.make 1 (Char.chr 0)) ^ (str lexbuf)}
    |       escape      {let cur = scan_escape (String.sub
                                  (Lexing.lexeme lexbuf) 1 1) in cur ^ (str lexbuf)}
    |       _       {let cur = Lexing.lexeme lexbuf in cur ^  (str lexbuf)} 

and chr =
    parse   '\''            {""}
    |       hex_escape  {let cur = scan_hex_escape (String.sub
                                      (Lexing.lexeme lexbuf) 2 2) in cur ^ (endchr lexbuf)}
    |       oct_escape  {let cur = scan_oct_escape (String.sub
                                      (Lexing.lexeme lexbuf) 1 3) in cur ^ (endchr lexbuf)}
    |       "\\0"       {(String.make 1 (Char.chr 0)) ^ (endchr lexbuf)}
    |       escape      {let cur = scan_escape (String.sub
                                  (Lexing.lexeme lexbuf) 1 1) in cur ^ (endchr lexbuf)}
    |       _       {let cur = Lexing.lexeme lexbuf in cur ^ (endchr lexbuf)} 

and endchr =
    parse   '\'' {""}
        | _ {""} (* miss a char, but likely in a comment *)
    
{

(* init: handle -> ()
**  Initialize lexer.
*)
let init hdl =
    current_handle := hdl 
}
