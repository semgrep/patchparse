(* --------------------------------------------------------------------- *)

(* change in data structure layout: requires . or ->, field name stays the
same *)

val data_layout_change : Diff.context_change -> Diff.context_change option

(* public to private or vice versa *)

val public_private : Diff.context_change -> Diff.context_change option

(* --------------------------------------------------------------------- *)

(* changes in the protocol *)

(* functions added or removed only.  ordering changed will require more
work. *)

val calls_added_or_removed : Diff.context_change -> Diff.context_change option

val calls_change : Diff.context_change -> Diff.context_change option
