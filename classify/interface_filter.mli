(* --------------------------------------------------------------------- *)

(* change in data structure layout: requires . or ->, field name stays the
   same *)

val data_layout_change : Taxonomy.interface_filter

(* public to private or vice versa *)

val public_private : Taxonomy.interface_filter

(* --------------------------------------------------------------------- *)

(* changes in the protocol *)

(* functions added or removed only.  ordering changed will require more
   work. *)

val calls_added_or_removed : Taxonomy.interface_filter

val calls_change : Taxonomy.interface_filter
