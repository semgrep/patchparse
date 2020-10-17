open Ce_visit

(* abstract line functions *)

let al_ce = rebuild_ce Ast_al.al_prim Ast_al.al_expr Ast_al.al_code

let have_al_ce =
  process_ce (function (x,y) -> x || y) (List.exists (function x -> x))
    Ast_al.have_al_prim Ast_al.have_al_expr Ast_al.have_al_code
