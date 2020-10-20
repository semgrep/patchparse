
(* Grouping evolutions in different categories *)

(* simple category (e.g., same_fn_drop_args) *)
type filter = Ce.ce -> bool

type interface_filter = Context_change.t -> Context_change.t option

(* less: we could put the Questions.result type also here *)


