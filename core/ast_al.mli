open Ast

val al_prim:   prim -> prim
val al_symbol: symbol -> symbol
val al_expr:   expr -> expr
val al_code:   code -> code

val have_al_prim:   prim -> bool
val have_al_symbol: symbol -> bool
val have_al_expr:   expr -> bool
val have_al_code:   code -> bool
