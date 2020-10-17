val process_all_files : Patch.t list ->
  (Diff.context_change list *
     (int * string (*path*) * string (*file*) * string (*region*))) list
