val process_all_files : Patch.t list ->
  (Context_change.t list *
     (int * string (*path*) * string (*file*) * string (*region*))) list
