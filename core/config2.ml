(* configurable things *)

(*
val real_fn : Ast.expr -> bool
val memory_mover : Ast.expr -> bool
*)

let real_fn = function(*no point to codify if and while - all meaning lost*)
    Ast.SYMBOL([Ast.IDENT("if",_)])
  | Ast.SYMBOL([Ast.IDENT("while",_)])
  | Ast.SYMBOL([Ast.IDENT("switch",_)])
  | Ast.SYMBOL([Ast.IDENT("sizeof",_)])
  | Ast.SYMBOL([Ast.IDENT("for",_)])
  | Ast.SYMBOL([Ast.IDENT("memcpy",_)])
  | Ast.SYMBOL([Ast.IDENT("memset",_)])
  | Ast.SYMBOL([Ast.IDENT("kmalloc",_)])
  | Ast.SYMBOL([Ast.IDENT("kfree",_)])
  | Ast.SYMBOL([Ast.IDENT("EXPORT_SYMBOL",_)]) -> false
  | Ast.SYMBOL([Ast.IDENT(s,_)])
      when Aux.substring "debug" (String.lowercase_ascii s) -> false
  | _ -> true

(* used to let a block memory mover match up with an assignment *)
let memory_mover = function
    Ast.SYMBOL([Ast.IDENT("memcpy",_)])
  | Ast.SYMBOL([Ast.IDENT("memset",_)]) -> true
  | _ -> false

let boring_ids id =
  let boring_fns id =
    (List.mem id
       ["return"; "dbg"; "err"; "dev_dbg"; "dev_err"; "dev_warn";
	 "printk"; "sprintf"; "sizeof";
	 "strcpy"; "strncpy"; "strcmp"; "strncmp"; "EXPORT_SYMBOL"]) ||
    Aux.substring "debug" (String.lowercase_ascii id) in
  boring_fns id ||
  (List.mem id
     ["break"; "continue"; "for"; "if"; "while"; "switch"; "int"; "void";
       "long"; "char"; "unsigned"; "static"; "goto"; "u8"; "u16"; "u32";
       "struct";
       "uint"; "uchar"; "ulong"; "u_int"; "u_char"; "u_long"; "byte"; "NULL";
       "else"; "__FUNCTION__"])

let boring_for_expify x =
  List.mem x
    ["int";"char";"void";"unsigned";"static";"inline";"u32";"goto";"struct";
      "u8";"u16";"extern"]

let boring_for_expify2 x =
  List.mem x ["return";"break";"continue";"goto"]
