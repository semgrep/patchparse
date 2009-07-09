val union : 'a list -> 'a list -> 'a list
val union_list : 'a list -> 'a list
val sorted_intersection : 'a list -> 'a list -> 'a list
val intersect : 'a list -> 'a list -> 'a list
val sorted_superset : 'a list -> 'a list -> bool
val sorted_difference : 'a list -> 'a list -> 'a list * 'a list * 'a list

val remove : 'a -> 'a list -> 'a list

val option_filter : ('a -> 'b option) -> 'a list -> 'b list

val option_partition : ('a -> 'b option) -> 'a list -> ('b list * 'a list)

val sum : int list -> int
val sumfloat : float list -> float
val get_min : int list -> int
val get_max : int list -> int

val normalize : string -> string

val safe_hash_find : ('a,'b) Hashtbl.t -> 'a -> (unit -> 'b) -> 'b
val safe_alist_find : ('a * 'b) list ref -> 'a -> (unit -> 'b) -> 'b

val substring : string -> string -> bool
val substring_index : string -> string -> int
