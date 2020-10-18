val collect :
  Eq_classes.change_table ->
  (Patch.id(*ver*) * Paths.dir(*dir*) * Paths.file list(*files*) * 
  Ce.ce list * float) list
