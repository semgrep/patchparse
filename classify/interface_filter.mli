(* --------------------------------------------------------------------- *)

(* change in data structure layout: requires . or ->, field name stays the
   same *)

val data_layout_change : Context_change.t -> Context_change.t option

(* public to private or vice versa *)

val public_private : Context_change.t -> Context_change.t option

(* --------------------------------------------------------------------- *)

(* changes in the protocol *)

(* functions added or removed only.  ordering changed will require more
   work. *)

val calls_added_or_removed : Context_change.t -> Context_change.t option

val calls_change : Context_change.t -> Context_change.t option
