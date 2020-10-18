(* set up data structures *)
module Config = Globals
module CC = Context_change
open Eq_classes
module CE = Ce

(* tables for filtered changes *)
(*let filters =
  [(Filter.addfn,"Add function call");
    (Filter.dropfn,"Drop function call");
    (Filter.new_fn_same_args,"Change function name, same arguments");
    (Filter.new_fn_drop_args,"Change function name, drop arguments");
    (Filter.new_fn_add_args,"Change function name, add arguments");
    (Filter.new_fn_add_and_drop_args,
     "Change function name, add and drop arguments");
    (Filter.same_fn_drop_args,"Same function name, drop arguments");
    (Filter.same_fn_add_args,"Same function name, add arguments");
    (Filter.same_fn_add_and_drop_args,
     "Same function name, add and drop arguments");
    (Filter.prototype_changed_visibility,
     "Function declaration same name, changed visibility");
    (Filter.prototype_changed_type,
     "Function declaration same name, changed type");
    (Filter.prototype_added_params,
     "Function declaration same name, added parameters");
    (Filter.prototype_dropped_params,
     "Function declaration same name, dropped parameters");
    (Filter.prototype_added_and_dropped_params,
     "Function declaration same name, added and dropped parameters");
    (Filter.prototype_name_changed,
     "Function declaration name changed, perhaps of no interest");
    (Filter.same_path_diff_field,"Same path, different field");
    (Filter.diff_path_same_field,"Different path, same field");
    (Filter.diff_path_and_field,"Different path and field");
    (Filter.addstorage,"Add use of a function result in an assignment or if");
    (Filter.dropstorage,
     "Drop use of a function result in an assignment or if");
    (Filter.add_return_value,
     "Convert a void return to a return of a value");
    (Filter.drop_return_value,
     "Convert a return of a value to a void return");
    (Filter.make_private,
     ("Make a structure field private (convert a field reference"^
     " to) a function call)"));
    (Filter.make_public,
     ("Make a structure field public (convert a function call to a"^
     " field reference)"));
    (Filter.errorify_return,
     ("Change a 0 return value to a negative constant or a 1 return"^
      " value to 0"));
    (Filter.unerrorify_return,
     "Change a 0 return value to 1 or a negative constant to 0");
    (Filter.errorify_value,
     ("change a test for a 0 return value to a test for a negative constant"^
      " or a test for a 1 return value to a test for a 0"));
    (Filter.unerrorify_value,
     ("change a test for a negative constant return value to a test"^
      " for a 0 or a test for a 0 return value to a test for a 1"))]*)

let interface_filters =
  [(Interface_filter.data_layout_change,
     "Change in structure data layout");
    (Interface_filter.public_private,
     "Change between public and private field");
    (Interface_filter.calls_change,"Any change in a function call");
    (Interface_filter.calls_added_or_removed,
     "Change in protocol: calls added or removed")]

let interface_sp_filters =
  [(Interface_filter.data_layout_change,
     "Change in structure data layout");
    (Interface_filter.public_private,
     "Change between public and private field");
    (Interface_filter.calls_change,"Any change in a function call")
      (* not supported for sp generation ;
    (Interface_filter.calls_added_or_removed,
     "Change in protocol: calls added or removed") *)]

let make_filter_tables filters =
  List.map
    (function (filter,filter_string) ->
      (filter,filter_string,ref 0,
       (Hashtbl.create(100) : worklist),    (* worklist table *)
       (Hashtbl.create(100) : change_table) (* change table - result *) 
      )
    )
    filters

let gsemi__interface_filter_tables = make_filter_tables interface_filters
let sp__interface_filter_tables = make_filter_tables interface_sp_filters


(* --------------------------------------------------------------------- *)

(* global tables, for considering all changes *)
let max_change_size = ref 0

let gsemi__change_worklist = (Hashtbl.create(200) : worklist)

(* --------------------------------------------------------------------- *)
(* build worklists *)

let eqworklists (changelist, (version, pathname, filename, region)) =
  (* the global worklist *)
  if not (!Config.noall)
  then
    List.iter
      (function change ->
	Eqclasses.eqworklists gsemi__change_worklist max_change_size
	 change (version, pathname, filename, region))
      changelist;
  let rec loop cc =
    let (cc,context) =
      match cc with
	CC.CC(change,context) ->
	  let (code,noncode) =
	    (* IMPT: if there is exp or code at the top level, there is only
	       exp or code underneath *)
	    List.partition
	      (function CC.CC(_,_) -> false | CC.CG(_,_) -> true)
	      context in
	  (CC.CC(change,code),noncode)
      | CC.CG(change,context) ->
	  failwith
	    (Printf.sprintf "generalized change at top level %s\n"
	       (Ce_unparse.ce2c change)) 
    in
    List.iter
      (function (filter,_,max_change_size,worklist,_) ->
	match filter (Diff.al_context_change cc) with
	  None -> ()
	| Some change ->
	    Eqclasses.eqworklists worklist max_change_size
	      (Diff.al_context_change change)
	      (version, pathname, filename, region))
      (if !Config.print_sp
      then sp__interface_filter_tables
      else gsemi__interface_filter_tables);
    List.iter loop context 
  in
  if not (!Config.nofilters)
  then
  List.iter loop changelist

let eqclasses keep_change_table =
  let big_change_table    = (Hashtbl.create(200) : change_table) in

  Eqclasses.eqclasses gsemi__change_worklist big_change_table max_change_size;
  let global_res =
    Questions.questions big_change_table keep_change_table false in
  let filter_res =
    List.map
      (function
	  (_,filter_string,max_change_size,worklist,change_table) ->
	    Eqclasses.eqclasses worklist change_table max_change_size;
	    (* change table here is always kept because it is used in
	       mktex.ml *)
	    (filter_string,change_table,
	     Questions.questions change_table true false))
      (if !Config.print_sp
      then sp__interface_filter_tables
      else gsemi__interface_filter_tables) in
  ((big_change_table,global_res),filter_res)
