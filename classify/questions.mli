type result =
  (string (*version*) *
   (Paths.dir (*dir*) * (Ce.ce * int (*count*)) list) list) list *
  (Paths.dir (*dir*) *
   (string (*version*) * (Ce.ce * int (*count*)) list) list) list *
  (Ce.ce *
   ((string (*version*) * Paths.dir (*dir*)) * int (*count*)) list)
    list list *
  ((Ce.ce * string) *
   ((string (*version*) * Paths.dir (*dir*)) * int (*count*)) list)
    list list *
  (Ce.ce *
   ((string (*version*) * Paths.dir (*dir*)) * int (*count*)) list) list *
  (Ce.ce *
   ((string (*version*) * int (*count*)) * int (*ver#*)) list) list

val show_result: result -> string

(* the percentage of changes that are multiversion *)
val percentage_multiver : int ref
val total_changes : int ref

val questions :
  Change_table.t (* change table *) ->
  bool (* is the change table needed after this phase? *) ->
  bool (* compute percentage_multiver? not for filtered tables *) ->
  result
