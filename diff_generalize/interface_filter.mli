(* --------------------------------------------------------------------- *)

(* change in data structure layout: requires . or ->, field name stays the
same *)

val data_layout_change : Cc.t -> Cc.t option

(* public to private or vice versa *)

val public_private : Cc.t -> Cc.t option

(* --------------------------------------------------------------------- *)

(* changes in the protocol *)

(* functions added or removed only.  ordering changed will require more
work. *)

val calls_added_or_removed : Cc.t -> Cc.t option

val calls_change : Cc.t -> Cc.t option
