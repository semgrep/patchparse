val select_diffs :
    (Diff.context_change list *
       (int * string (*path*) * string (*file*) * string (*region*))) list  ->
	 (Diff.context_change list *
	    (int * string (*path*) * string (*file*) * string (*region*))) list
