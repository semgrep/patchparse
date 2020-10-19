
(* this used to be in init.ml *)
val process_file: 
  (Patch.id * Paths.dir * Paths.file -> 
   int (* start_line *) -> Patch.region -> 
  string (* minus part *) -> string (* plus part *) -> 
  'b list) ->
  Patch.t -> 'b list

val hunks_of_patch: Patch.t -> Patch.hunk list
