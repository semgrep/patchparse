(* Function added *)
val addfn : Ce.ce -> bool

(* Function dropped *)
val dropfn : Ce.ce -> bool

(* Function change, no change of argument *)
val new_fn_same_args : Ce.ce -> bool

(* Function change, drop arguments, ignore argument order *)
val new_fn_drop_args : Ce.ce -> bool

(* Function change, add arguments, ignore argument order *)
val new_fn_add_args : Ce.ce -> bool
    
(* Function change, add and drop arguments, ignore argument order *)
val new_fn_add_and_drop_args : Ce.ce -> bool
    
(* Function same, drop arguments, ignore argument order *)
val same_fn_drop_args : Ce.ce -> bool
    
(* Function same, add arguments, ignore argument order *)
val same_fn_add_args : Ce.ce -> bool
    
(* Function same, add and drop arguments, ignore argument order *)
val same_fn_add_and_drop_args : Ce.ce -> bool

val contains_exp : Ast.code list -> bool (* for function parameters *)

(* Prototype same name, changed visibility *)
val prototype_changed_visibility : Ce.ce -> bool

(* Prototype same name, changed type *)
val prototype_changed_type : Ce.ce -> bool

(* Prototype same name, added parameters *)
val prototype_added_params : Ce.ce -> bool

(* Prototype same name, dropped parameters *)
val prototype_dropped_params : Ce.ce -> bool

(* Prototype same name, added and dropped parameters *)
val prototype_added_and_dropped_params : Ce.ce -> bool

(* Prototype name changed, perhaps of no interest *)
val prototype_name_changed : Ce.ce -> bool

(* Fields, same path, different field *)
val same_path_diff_field : Ce.ce -> bool
  
(* Fields, different path, same field *)
val diff_path_same_field : Ce.ce -> bool
    
(* Fields, different path and field *)
val diff_path_and_field : Ce.ce -> bool
  
(* Add the storage or testing of the result of a function call *)
val addstorage : Ce.ce -> bool
    
(* Drop the storage or testing of the result of a function call *)
val dropstorage : Ce.ce -> bool

(* convert a void return to a return of a value *)
val add_return_value : Ce.ce -> bool

(* convert a return of a value to a void return *)
val drop_return_value : Ce.ce -> bool

(* convert a dereference to a function call *)
val make_private : Ce.ce -> bool
    
(* convert a function call to a dereference *)
val make_public : Ce.ce -> bool

(* change a 0 return value to a negative constant or a 1 return value to 0 *)
val errorify_return : Ce.ce -> bool

(* change a 0 return value to 1 or a negative constant to 0 *)
val unerrorify_return : Ce.ce -> bool

(* change a test for a 0 return value to a test for a negative constant or a
   test for a 1 return value to a test for a 0 *)
val errorify_value : Ce.ce -> bool

(* change a test for a negative constant return value to a test for a 0 or a
   test for a 0 return value to a test for a 1 *)
val unerrorify_value : Ce.ce -> bool
