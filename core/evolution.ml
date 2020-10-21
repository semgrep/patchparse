type t = 
  Patch.id(*ver*) * Paths.dir(*dir*)(*TODO or dir version?*) *
  Paths.file list(*files*) * Change.ce list * float (*weight*)
 [@@deriving show { with_path = false }]
