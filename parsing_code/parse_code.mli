
val parse: Patch.id -> string -> Ast.code list list option

(* internals, used only to debug intermediate ast0 *)
val parse_ast0: Patch.id -> string -> Ast0.code list