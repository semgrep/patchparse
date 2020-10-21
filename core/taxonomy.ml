
(* Grouping evolutions in different categories *)

(* simple category (e.g., same_fn_drop_args) *)
type filter = Ce.ce -> bool

type interface_filter = Change_tree.t -> Change_tree.t option

(* less: we could put the Questions.result type also here *)


