(* Function added *)
val addfn : Taxonomy.filter

(* Function dropped *)
val dropfn : Taxonomy.filter

(* Function change, no change of argument *)
val new_fn_same_args : Taxonomy.filter

(* Function change, drop arguments, ignore argument order *)
val new_fn_drop_args : Taxonomy.filter

(* Function change, add arguments, ignore argument order *)
val new_fn_add_args : Taxonomy.filter

(* Function change, add and drop arguments, ignore argument order *)
val new_fn_add_and_drop_args : Taxonomy.filter

(* Function same, drop arguments, ignore argument order *)
val same_fn_drop_args : Taxonomy.filter

(* Function same, add arguments, ignore argument order *)
val same_fn_add_args : Taxonomy.filter

(* Function same, add and drop arguments, ignore argument order *)
val same_fn_add_and_drop_args : Taxonomy.filter



(* change name or add or drop arguments *)
val any_change_in_call : Taxonomy.filter



val contains_exp : Ast.code list -> bool (* for function parameters *)

(* Prototype same name, changed visibility *)
val prototype_changed_visibility : Taxonomy.filter

(* Prototype same name, changed type *)
val prototype_changed_type : Taxonomy.filter

(* Prototype same name, added parameters *)
val prototype_added_params : Taxonomy.filter

(* Prototype same name, dropped parameters *)
val prototype_dropped_params : Taxonomy.filter

(* Prototype same name, added and dropped parameters *)
val prototype_added_and_dropped_params : Taxonomy.filter

(* Prototype name changed, perhaps of no interest *)
val prototype_name_changed : Taxonomy.filter

(* Fields, same path, different field *)
val same_path_diff_field : Taxonomy.filter

(* Fields, different path, same field *)
val diff_path_same_field : Taxonomy.filter

(* Fields, different path and field *)
val diff_path_and_field : Taxonomy.filter

(* Add the storage or testing of the result of a function call *)
val addstorage : Taxonomy.filter

(* Drop the storage or testing of the result of a function call *)
val dropstorage : Taxonomy.filter

(* convert a void return to a return of a value *)
val add_return_value : Taxonomy.filter

(* convert a return of a value to a void return *)
val drop_return_value : Taxonomy.filter

(* convert a dereference to a function call *)
val make_private : Taxonomy.filter

(* convert a function call to a dereference *)
val make_public : Taxonomy.filter

(* change a 0 return value to a negative constant or a 1 return value to 0 *)
val errorify_return : Taxonomy.filter

(* change a 0 return value to 1 or a negative constant to 0 *)
val unerrorify_return : Taxonomy.filter

(* change a test for a 0 return value to a test for a negative constant or a
   test for a 1 return value to a test for a 0 *)
val errorify_value : Taxonomy.filter

(* change a test for a negative constant return value to a test for a 0 or a
   test for a 0 return value to a test for a 1 *)
val unerrorify_value : Taxonomy.filter
