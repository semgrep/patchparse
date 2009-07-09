val process_all_files : (int * string list) list ->
  (Diff.context_change list *
     (int * string (*path*) * string (*file*) * string (*region*))) list
