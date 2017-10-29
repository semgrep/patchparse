module CE = Ce

(* We consider the following questions:

1. For each version, what are the changes that occur in the various
   directories?

   From the existing data, we first extract the information associated with
   each version.  For a version, we collect the affected directories, and
   for each one make a table mapping each change to the number of times it
   occurs.

   (version * (directory * (change * count) list) list) list

   This is for everything that satisfies same_threshold and file_threshold.

2. For each directory, what are the changes that occur across the various
   versions?

   (directory * (version * (change * count) list) list) list

   This is for everything that satisfies same_threshold and file_threshold.

3. For a given change in how many directories and versions does it occur?

   (change * (directory * (version * count) list) list) list

   This is only for things that satisfy directory_threshold.

4. For a given change in how many directories and versions does it occur?

   (change * (directory * (version * count) list) list) list

   This is only for things that satisfy version_threshold.
*)
(* -------------------------------------------------------------------- *)
(* processing the collected data *)

let reorganize pre_fn get_header get_sub1 get_sub2
    (intable: Eqclasses.change_table_type) outtable =
  Hashtbl.iter
    (function change ->
      function vdc ->
	let vdc = pre_fn vdc in
	List.iter
	  (function ((version,dir),count,files, regions, not_al_changeset) ->
	    let data = (change,version,dir,count) in
	    let header = get_header data in
	    let sub1 = get_sub1 data in
	    let sub2 = get_sub2 data in
	    let cell =
	      try Hashtbl.find outtable header
	      with Not_found ->
		let cell = ref [] in
		Hashtbl.add outtable header cell;
		cell in
	    let inner_cell =
	      try List.assoc sub1 !cell
	      with Not_found ->
		let inner_cell = ref [] in
		cell := (sub1,inner_cell) :: !cell;
		inner_cell in
	    inner_cell := sub2 :: !inner_cell)
	  vdc)
    intable

let id x = x

let git_version_restrict vdc =
  let h = Hashtbl.create(List.length vdc) in
  List.iter
    (function ((version,dir),count,files, regions, not_al_changeset) ->
      let cell =
	try Hashtbl.find h version
	with Not_found ->
	  let cell = ref 0 in
	  Hashtbl.add h version cell;
	  cell in
      cell := !cell + count)
    vdc;
  List.filter
    (function ((version,dir),count,files, regions, not_al_changeset) ->
      let total_count = Hashtbl.find h version in
      if !total_count >= !Config.same_threshold
      then true
      else false)
    vdc

let make_version_table in_table out_table =
  reorganize git_version_restrict
    (function (change,version,dir,count) -> version)
    (function (change,version,dir,count) -> dir)
    (function (change,version,dir,count) -> (change,count))
    in_table out_table

let make_directory_table in_table out_table =
  reorganize id
    (function (change,version,dir,count) -> dir)
    (function (change,version,dir,count) -> version)
    (function (change,version,dir,count) -> (change,count))
    in_table out_table

let make_multidir_table in_table out_table =
  reorganize id
    (function (change,version,dir,count) -> change)
    (function (change,version,dir,count) -> version)
    (function (change,version,dir,count) -> (dir,count))
    in_table out_table;
  (* drop the entries that are not in enough directories *)
  let entries =
    Hashtbl.fold
      (function change ->
	function info ->
	  function rest ->
	    let versions =
	      (List.length
		 (List.concat
		    (List.map (function (version,dir_cts) -> !dir_cts)
		       !info))) in
	    (change,versions,info)::rest)
      out_table [] in
  Hashtbl.clear out_table;
  List.iter
    (function (change,versions,info) ->
      if versions >= !Config.directory_threshold
      then Hashtbl.add out_table change info)
    entries

let percentage_multiver = ref 0
let total_changes = ref 0

(* -------------------------------------------------------------------- *)
(* Postprocessing to put the collected data into a form suitable for
printing.  Want to sort the innermost list, and then apply create_overwrite
to create value appropriate for making bars. *)

let postprocess_table process_key process_sub1 table =
  Hashtbl.fold
    (function key ->
      function info ->
	function rest ->
	  (process_key key,
	   List.map
	     (function (sub1,sub2) ->
	       (process_sub1 sub1,sub2))
	     (List.sort compare
		(List.map
		   (function (sub1,sub2) ->
		     (sub1,
		      (List.sort
			 (function (key1,ct1) ->
			   function (key2,ct2) ->
			     compare (ct1,key1) (ct2,key2))
			 !sub2)))
		   !info)))
	  :: rest)
    table []

let postprocess_version_table table =
  postprocess_table
    (function version -> Config.get_version version)
    (function dir -> dir)
    table

let postprocess_directory_table table =
  postprocess_table
    (function dir -> dir)
    (function version -> Config.get_version version)
    table

(* try to collect similar versions together, so we don't end up with too
many colors in the final graph. Then cluster when overlapping commits. *)

let postprocess_md_table table =
  let table_data =
    Hashtbl.fold
       (function change ->
	 function info ->
	   function (rest : (('change *
				((int(*v*) * string(*d*)) * int(*ct*)) list)
			       list)) ->
	     (change,
	      List.sort compare
		(List.concat
		   (List.map
		      (function (version,dircts) ->
			List.map
			  (function (dir,ct) -> ((version,dir),ct))
			  !dircts)
		      !info)))
	     :: rest)
       table [] in
  let sort_by_version =
    List.sort
      (function (change1,data1) ->
	function (change2,data2) ->
	  match (data1,data2) with
	    (((verdir1,_)::_),((verdir2,_)::_)) ->
	      compare (verdir1,change1) (verdir2,change2)
	  | _ -> failwith "not possible")
      table_data in
  let convert_version_numbers =
    List.map
      (function (change,data) ->
	(change,
	 List.map
	   (function ((version,data),ct) ->
	     ((Config.get_version version,data),ct))
	   data))
      sort_by_version in
  let intersects l1 l2 = List.exists (fun x -> List.mem x l2) l1 in
  let versions_of_all =
    List.map
      (function entry ->
	(entry, List.map (function ((ver,data),ct) -> ver) (snd entry)))
      convert_version_numbers in
  (* cluster overlapping commits *)
  let rec merge_intersecting_versions = function
      [] -> []
    | [x] -> [[fst x]]
    | (x,versions)::xs ->
	let (others,rest) =
	  List.partition
	    (function (y,versions2) -> intersects versions versions2)
	    xs in
	(x::List.map fst others) :: merge_intersecting_versions rest in
  merge_intersecting_versions versions_of_all

let stddev l =
  let ave = (float_of_int(Aux.sum l)) /. (float_of_int(List.length l)) in
  let diffs =
    List.map
      (function x ->
	let x = float_of_int x in
	(x -. ave) *. (x -. ave))
      l in
  let sumdiffs = Aux.sumfloat diffs in
  let arg = sumdiffs /. ((float_of_int(List.length l)) -. 1.0) in
  int_of_float (sqrt arg)

(* first list is like multidir result but not sorted because that doesn't
matter here.  second list maps versions to the number of changes for the
version and is used to make the graph *)
let postprocess_mv_table table compute_percentage_multiver =
  let table_data =
    Hashtbl.fold
       (function change ->
	 function info ->
	   function (rest :
		       (('change *
			   ((int(*v*) * string(*d*)) * int(*ct*)) list *
			   ((int(*v*) * int(*ct*)) * int(*v#*)) list)
			  list)) ->
	     (change,
	      List.sort compare
		(List.concat
		   (List.map
		      (function (version,dircts) ->
			List.map
			  (function (dir,ct) -> ((version,dir),ct))
			  !dircts)
		      !info)),
	      List.sort compare
		(List.map
		   (function (version,dircts) ->
		     ((version,
		       Aux.sum (List.map (function (dir,ct) -> ct) !dircts)),
		      version))
		   !info))
	     :: rest)
       table [] in
  let sort_by_version =
    List.sort
      (function (change1,vdc_data1,vcv_data1) ->
	function (change2,vdc_data2,vcv_data2) ->
	  match (vcv_data1,vcv_data2) with
	    ((((ver1,ct1),_)::_),(((ver2,ct2),_)::_)) ->
	      compare ((ver1,0-ct1),change1) ((ver2,0-ct2),change2)
	  | _ -> failwith "not possible")
      table_data in
  let multiver = ref 0 in
  let convert_version_numbers =
    Aux.option_filter
      (function (change,data1,data2) ->
	let cts =
	  List.map (function ((version,ct),version_copy) -> ct) data2 in
	if stddev cts > 1
	then
	  begin
	    multiver := !multiver + 1;
	    Some ((change,
		   List.map
		     (function ((version,data),ct) ->
		       ((Config.get_version version,data),ct))
		     data1),
		  (change,
		   List.map
		     (function ((version,ct),version_copy) ->
		       ((Config.get_version version,ct),version_copy))
		     data2))
	  end
	else None)
      sort_by_version in
  if compute_percentage_multiver
  then percentage_multiver := (!multiver * 100) / !total_changes;
  List.split(convert_version_numbers)

let postprocess_tables compute_percentage_multiver
    version_table directory_table multidir_table multiver_table =
  let (mv1,mv2) =
    postprocess_mv_table multiver_table compute_percentage_multiver in
  (postprocess_version_table version_table,
   postprocess_directory_table directory_table,
   postprocess_md_table multidir_table,
   mv1,mv2)

(* -------------------------------------------------------------------- *)
(* Entry point *)

(* make them all so we can easily have a summary *)
let mk_questions (in_table: Eqclasses.change_table_type) keep_change_table
    compute_percentage_multiver
    version_table directory_table multidir_table multiver_table =
  (*if List.mem Config.VERSION !Config.desired_info
     then*) make_version_table in_table version_table;
  (*if List.mem Config.DIRECTORY !Config.desired_info
     then*) make_directory_table in_table directory_table;
  (*if List.mem Config.MULTIDIR !Config.desired_info
     then*) make_multidir_table in_table multidir_table;
  if not keep_change_table
  then Hashtbl.clear in_table;
  postprocess_tables compute_percentage_multiver version_table directory_table
    multidir_table multiver_table

type result =
    (string (*version*) *
       (string (*dir*) * (CE.ce * int (*count*)) list) list) list *
      (string (*dir*) *
	 (string (*version*) * (CE.ce * int (*count*)) list) list) list *
      (CE.ce *
	 ((string (*version*) * string (*dir*)) * int (*count*)) list)
      list list *
      (CE.ce *
	 ((string (*version*) * string (*dir*)) * int (*count*)) list) list *
      (CE.ce *
	 ((string (*version*) * int (*count*)) * int (*ver#*)) list) list

let questions change_table keep_change_table compute_percentage_multiver =
  mk_questions change_table keep_change_table compute_percentage_multiver
    (Hashtbl.create 100) (Hashtbl.create 100)
    (Hashtbl.create 100) (Hashtbl.create 100)
