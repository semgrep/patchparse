module Config = Globals

let logger = Logging.get_logger [__MODULE__]


let parse (version : Patch.id) s =
  (* put the __line at the beginning of the line *)
  let s =
    Str.global_replace
      (Str.regexp "\\([^\n]*\\)___line=\\([0-9]+\\)")  "___line=\\2 \\1" s in

  try
    let lexbuf = Lexing.from_string s in
    Clexer.init (true, "", "", 0, 0, stderr, s);
    (*Clexer.set_current_line line_number;*)
    let parsed = Cparser2.interpret Clexer.initial lexbuf in
    Some (No_modifs.drop_outer (Ast0_to_ast.convert parsed))
  with
  | Failure x ->
    let (Patch.Id version) = version in
    let ver =
      try Hashtbl.find Config.version_table version
      with Not_found -> string_of_int version
    in
    let ver = if String.length ver < 12 then ver else String.sub ver 0 12 in
    logger#error "%s: %s\n   %s\n" ver x s;
    (*Printexc.print_backtrace stdout;*)
    None
  | Parsing.Parse_error -> 
    logger#error "Parse error";
    None
(*  | x -> None*)
