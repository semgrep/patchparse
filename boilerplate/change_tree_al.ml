open Change_tree

let rec al_context_change = function
  | CC ( ce (* the change *) , context_change_list) -> 
    CC (Ce_al.al_ce ce, List.map al_context_change context_change_list)
  | CG (ce (* a change generalized with EXP or CODE *), context_change_list) ->
    CG (Ce_al.al_ce ce, List.map al_context_change context_change_list)

let rec have_al_context_change = function
  | CC (ce (* the change *) , context_change_list) -> 
    (Ce_al.have_al_ce ce ||
     (List.exists have_al_context_change context_change_list))
  | CG (ce (* a change generalized with EXP or CODE *), context_change_list) ->
    (Ce_al.have_al_ce ce ||
     (List.exists have_al_context_change context_change_list))
