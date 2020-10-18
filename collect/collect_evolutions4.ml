module CE = Ce
(* we only do context changes for the moment *)

(* In principle, we want to intersect the set of files associated with
every change with the set of files associated with each other change.  To
be a little bit more efficient, we consider only the changes that have at
least one file in common with the current one, so that the intersection
will at least not be empty.  The files_to_changes hash table is used to
detect these changes.  The changes_seen hash table is used so that we don't
consider any pair of changes more than once. *)

(* --------------------------------------------------------------------- *)
(* collect changes and files for each version/version&directory *)

(* Collected doesn't contain entries for the very common case where the
distance is 1.  Thus, if a pair of changes is not found here (or in
tmp_distace_table), then the distance is considered to be 1.0 *)

(* key is either version only or a pair of version and dir *)

(* We start with the following table:
     key -> change * files list

Then, for each instance of key, we build the following tables, in order:
     1. change -> files
     2. (change * change) -> distance
We use the files of the first to produce the second, and subsequently use
the first to get a list of all of the changes to build the equivalence
classes. *)

let version_dir_change_files =
  (Hashtbl.create(200) :
     ((Patch.id (* version *) * string (* dir *)),
      (CE.ce * (string list ref (* sorted files *) *
		  string list ref (* referenced symbols *))) list ref)
     Hashtbl.t)

let version_change_files =
  (Hashtbl.create(200) :
     (Patch.id (* version *),
      (CE.ce * (string list ref (* sorted files *) *
		  string list ref (* referenced symbols *))) list ref)
     Hashtbl.t)

let version_dir_key (version,dir) = (version,dir)
let (version_key: Patch.id * string -> Patch.id) = 
  fun (version,dir) -> version

let create_version_dir_change_files 
    table 
    (get_key: Patch.id * string -> Patch.id)
    (change_table: Eq_classes.change_table_type) 
  =
  Hashtbl.iter
    (function change ->
      function info ->
	List.iter
	  (function ((version,dir),ct,files, regions, not_al_changeset) ->
	    let key = get_key (version,dir) in	    
	    let cell =
	      Aux.safe_hash_find table key (function _ -> ref []) in
	    let (cell1,cell2) =
	      Aux.safe_alist_find cell change (function _ -> (ref [],ref[])) in
	    cell1 := Aux.union files !cell1;
	    cell2 :=
	      Aux.union
		(List.fold_left Aux.union []
		   (List.map CeH.get_characters not_al_changeset))
		!cell2)
	  info)
    change_table;
  (* sort files associated with each change *)
  Hashtbl.iter
    (function key ->
      function info ->
	List.iter
	  (function (change,(filelist,chars)) ->
	    filelist := List.sort compare !filelist;
	    chars := List.sort compare !chars)
	  !info)
    table

(* --------------------------------------------------------------------- *)
(* compute the distances between pairs of changes for a given key *)

let distance_table =
  (Hashtbl.create(200) : (CE.ce * CE.ce, float) Hashtbl.t)

let normalize x y = if compare x y < 0 then (x,y) else (y,x)

(* for arbitrary evolutins *)
let compute_distances key_change_files key =
  Hashtbl.clear distance_table;
  let worklist = !(Hashtbl.find key_change_files key) in
  let rec loop = function
      [] -> ()
    | (change,(files,chars))::rest ->
	loop rest;
	let files = !files in
	List.iter
	  (function (wchange,(wfiles,wchars)) ->
	    let file_weight =
	      let wfiles = !wfiles in
	      let (filesonly,both,wfilesonly) =
		Aux.sorted_difference files wfiles in
	      let lenboth = List.length both in
	      let lenfilesonly = List.length filesonly in
	      let lenwfilesonly = List.length wfilesonly in
	      (float_of_int(lenfilesonly + lenwfilesonly)) /.
	      (float_of_int(lenfilesonly + lenboth + lenwfilesonly)) in
	    let commonality_weight =
	      let (conly,ccboth,wconly) =
		Aux.sorted_difference !chars !wchars in
	      let lconly = List.length conly in
	      let lwconly = List.length wconly in
	      let diff = min lconly lwconly in
	      let lccboth = List.length ccboth in
	      Printf.printf "lconly %d lwconly %d lccboth %d\n"
		lconly lwconly lccboth;
	      if diff + lccboth = 0
	      then file_weight
	      else
		(float_of_int diff) /. (float_of_int(diff + lccboth)) in
	    let tuple = normalize change wchange in
	    let weight = min file_weight commonality_weight in
	    (*if not(weight = 1.0)
	    then*)
	      (Printf.printf
		"The distance between the following changes is %f %f %f\n"
		file_weight commonality_weight weight;
	      Printf.printf "%s\nin\n" (Ce_unparse.ce2c change);
	      List.iter (function s -> Printf.printf "  %s\n" s) files;
	      Printf.printf "%s\nin\n" (Ce_unparse.ce2c wchange);
	      List.iter (function s -> Printf.printf "  %s\n" s) !wfiles;
	      Printf.printf "strings %s, wstrings %s\n"
	        (String.concat " " !chars)
	        (String.concat " " !wchars);
	      Printf.printf "***************\n\n";

	      Hashtbl.add distance_table tuple weight))
	  rest in
  loop worklist


(* trying to focus on function protocols *)
let compute_distances key_change_files key =
  Hashtbl.clear distance_table;
  let worklist = !(Hashtbl.find key_change_files key) in
  let rec loop = function
      [] -> ()
    | (change,(files,chars))::rest when CeH.function_change change ->
	Printf.printf "function change: %s\n" (Ce_unparse.ce2c change);
	loop rest;
	let files = !files in
	let (old_functions,new_functions) = CeH.get_function_names change in
	let old_functions = List.sort compare old_functions in
	let new_functions = List.sort compare new_functions in
	List.iter
	  (function (wchange,(wfiles,wchars)) when CeH.function_change wchange->
	    let file_weight =
	      let wfiles = !wfiles in
	      let (filesonly,both,wfilesonly) =
		Aux.sorted_difference files wfiles in
	      let lenboth = List.length both in
	      let lenfilesonly = List.length filesonly in
	      let lenwfilesonly = List.length wfilesonly in
	      (float_of_int(lenfilesonly + lenwfilesonly)) /.
	      (float_of_int(lenfilesonly + lenboth + lenwfilesonly)) in
	    let commonality_weight =
	      let (conly,ccboth,wconly) =
		Aux.sorted_difference !chars !wchars in
	      let lconly = List.length conly in
	      let lwconly = List.length wconly in
	      let diff = min lconly lwconly in
	      let lccboth = List.length ccboth in
	      Printf.printf "lconly %d lwconly %d lccboth %d\n"
		lconly lwconly lccboth;
	      if diff + lccboth = 0
	      then file_weight
	      else
		(float_of_int diff) /. (float_of_int(diff + lccboth)) in
	    let tuple = normalize change wchange in
	    let weight = min file_weight commonality_weight in
	    let (old_wfunctions,new_wfunctions) =
	      CeH.get_function_names wchange in
	    let old_wfunctions = List.sort compare old_wfunctions in
	    let new_wfunctions = List.sort compare new_wfunctions in
	    let weight =
	      if List.length old_functions = 1 &&
		List.length old_wfunctions = 1 &&
		old_functions = new_functions &&
		old_wfunctions = new_wfunctions
	      then 1.0
	      else
		if List.length (Aux.union old_functions old_wfunctions) > 1 ||
		  List.length (Aux.union new_functions new_wfunctions) > 1
		then weight *. weight
		else weight in
	    if not(weight = 1.0)
	    then
	      ((*Printf.printf
		"The distance between the following changes is %f %f %f\n"
		file_weight commonality_weight weight;
	      Printf.printf "%s\nin\n" (CE.ce2c change);
	      List.iter (function s -> Printf.printf "  %s\n" s) files;
	      Printf.printf "%s\nin\n" (CE.ce2c wchange);
	      List.iter (function s -> Printf.printf "  %s\n" s) !wfiles;
	      Printf.printf "strings %s, wstrings %s\n"
	        (String.concat " " !chars)
	        (String.concat " " !wchars);
	      Printf.printf "***************\n\n";
*)
	      Hashtbl.add distance_table tuple weight)
	    | _ -> ())
	  rest
    | _::rest -> loop rest in
  loop worklist

(* --------------------------------------------------------------------- *)

let get_min_dist worklist =
  let min_pair = ref (None : (CE.ce * CE.ce) option) in
  (* prevents completely disjoint sets, which don't appear anyway *)
  let min_val = ref 1.0 in
  Hashtbl.iter
    (function (change1,change2) ->
      function weight ->
	if List.mem change1 worklist && List.mem change2 worklist &&
	  weight < !min_val
	then
	  begin
	    min_pair := Some(change1,change2);
	    min_val := weight
	  end)
    distance_table;
  (!min_pair,!min_val)

(* returns equivalence classes for key *)
let make_classes key_change_files key =
  let changes =
    List.map (function (c,_) -> c) (!(Hashtbl.find key_change_files key)) in
  let worklist = ref changes in
  let classes =
    ref ([] : (CE.ce list ref * float ref (*max dist*) *
		 float ref (*sz*)) list) in
  let rec loop ((elems,curmax,sz) as curclass) =
    match !worklist with
      [] -> () (* done *)
    | _ ->
	(* try to add a change to the current class (remaining elements
	are too far from the other classes) *)
	(* collect everything that has the same score, because
	   otherwise picking one will probably preclude adding
	   the others.  might not even be the same class in every
	   case. *)
	let min_max = ref 2.0 in
	let min_max_change_class = ref [] in
	List.iter
	  (function change ->
	    let maxval = ref (-1.0) in
	    let money = (1.0 -. !curmax) /. !sz in
	    List.iter
	      (function change1 ->
		let dist =
		  try Hashtbl.find distance_table (normalize change change1)
		  with Not_found -> 1.0 in
		if dist > !maxval
		then maxval := dist)
	      !elems;
	    if !maxval > -1.0 && !maxval < money
	    then
	      if !maxval < !min_max
	      then
		begin
		  min_max := !maxval;
		  min_max_change_class := [(change,curclass)]
		end
	      else
		if !maxval = !min_max
		then
		  min_max_change_class :=
		    (change,curclass)::!min_max_change_class)
	  !worklist;
	(match !min_max_change_class with
	  [] -> (* found nothing, try to make a new class *)
	    make_a_new_class()
	| _ ->
	    List.iter
	      (function (change,(elems,curmax,sz)) ->
		elems := change::!elems;
		Printf.printf "%f: %s\n" !min_max (Ce_unparse.ce2c change);
		curmax := !min_max;
		sz := !sz +. 1.0;
		worklist := Aux.remove change !worklist)
	      !min_max_change_class;
	    loop curclass)
  and make_a_new_class _ =
    let (min_pair,min_val) = get_min_dist !worklist in
    (match (min_pair,min_val) with
      (Some(change1,change2),dist) ->
	Printf.printf "making a class for %s and %s\n"
	   (Ce_unparse.ce2c change1) (Ce_unparse.ce2c change2);
	(* sz is initially 1 with 2 because it represents how
	   many elements have grouped with the center. or maybe
	   because 2 didn't let enough things group together *)
	let curclass = (ref [change1;change2],ref dist,ref 1.0) in
	classes := curclass::!classes;
	worklist := Aux.remove change1 (Aux.remove change2 !worklist);
	loop curclass
    | (None,_) -> ()) (* failed to add anything new *) in 
  make_a_new_class();
  List.map (function (elems,curmax,sz) -> (!elems,!curmax)) !classes

(* --------------------------------------------------------------------- *)
(* Augment classes with files (could have been done in the previous step, but
separated for simplicity) *)

let augment_classes key_table key (classes : (CE.ce list * float) list) =
  let get_files curclass : string list =
    List.sort compare
      (List.fold_left
	 (function rest ->
	   function change ->
	     let (files,_) =
	       (List.assoc change (!(Hashtbl.find key_table key))) in
	     let files = !files in
	     Aux.union files rest)
	 [] curclass) in
  List.map (function (c,weight) -> (get_files c,c,weight)) classes

(* --------------------------------------------------------------------- *)
(* Process all versions *)

let process_versions (version_dir_change_files :
     ('key,(CE.ce * (string list ref (* sorted files *) *
		       string list ref (* referenced symbols *))) list ref)
			Hashtbl.t) =
  Hashtbl.fold
    (function key ->
      function _ ->
	function rest ->
	  compute_distances version_dir_change_files key;
	  let classes =
	    augment_classes version_dir_change_files key
	      (make_classes version_dir_change_files key) in
	  (List.map
	     (function (files,changes,weight) -> (key,files,changes,weight))
	     classes)
	  @ rest)
    version_dir_change_files []

(* --------------------------------------------------------------------- *)
(* Entry point *)

let collect change_table =
  create_version_dir_change_files version_change_files
    version_key change_table;
  List.map
    (function (version,files,changes,weight) ->
      (version,"",files,changes,weight))
    (List.sort compare (process_versions version_change_files))
