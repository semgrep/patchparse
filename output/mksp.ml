module CE = Ce

(* -------------------------------------------------------------------- *)
(* Print the data in the VERSION or DIRECTORY cases *)

let split_git_version version =
  match Str.split (Str.regexp " ") version with
    git_code :: rest -> let rest = String.concat " " rest in (git_code,rest)
  | _ -> failwith "bad version"

(* -------------------------------------------------------------------- *)
(* Semantic patch stuff *)
	
let cocci_prolog o =
  Printf.fprintf o "virtual invalid\n\n"

let ct = ref 0 (* semantic patch counter *)

(* -------------------------------------------------------------------- *)
(* Make file stuff *)
	
let pre_print_to_get_files o ct =
  if !Config.git && not (!Config.gitpatch)
  then
    begin
      Printf.fprintf o "\nrule%d:\n" ct;
      let dir = Sys.getcwd() in
      Printf.fprintf o "\t/bin/rm -f %s/%s/$(DEST)/index\n"
	dir !Config.out_dir;
      Printf.fprintf o "\t/usr/bin/touch %s/%s/$(DEST)/index\n"
	dir !Config.out_dir;
      Printf.fprintf o "\tcd %s\n" !Config.gitdir
    end

let mkname file =
  String.concat "__" (Str.split (Str.regexp "/") file)

let print_to_get_files o ct code =
  if !Config.git && not (!Config.gitpatch)
  then
    begin
      let diffs =
	Aux.cmd_to_list
	  (Printf.sprintf "cd %s; /usr/bin/git show %s | /bin/grep ^diff"
	     !Config.gitdir code) in
      let files =
	List.map
	  (function diff ->
	    match Str.split (Str.regexp " b/") diff with
	      _::file::_ -> file
	    | _ -> failwith "bad diff")
	  diffs in
      let dir = Sys.getcwd() in
      List.iter
	(function file ->
	  Printf.fprintf o
	    "\tgit cat-file blob %s^:%s > %s/%s/$(DEST)/%s\n\tgit cat-file blob %s:%s > %s/%s/$(DEST)/%s.res\n"
	    code file dir !Config.out_dir (mkname file)
	    code file dir !Config.out_dir (mkname file);
	  Printf.fprintf o "\techo %s %s.res >> %s/%s/$(DEST)/index\n"
	    (mkname file) (mkname file) dir !Config.out_dir)
	files
    end

(* -------------------------------------------------------------------- *)
(* Printing *)
	
let file_data sp_file get_files
    (printer_sp : int -> 'change -> string)
    pre_print_to_get_files print_to_get_files
    ((version_table,dir_table,multidir_table,multiver_table1,multiver_table2):
       Questions.result) =  
  let pcm data = (* multi git code *)
      List.iter
        (function (change,data) ->
          ct := !ct + 1;
	  pre_print_to_get_files get_files !ct;
	  let comment =
	    let rec loop prev = function
                [] -> []
              | ((version,dir),count)::rest ->
                  let version = CE.clean version in
                  let (git_code,grest) = split_git_version version in
		  (* there is an entry for each dir, but don't print them *)
                  if prev = git_code
                  then loop prev rest
		  else
		    begin
		      print_to_get_files get_files !ct git_code;
		      let unused_tokens =
			try !(Hashtbl.find Eqclasses.version_unused_table
				version)
			with Not_found -> 0 in
		      let front =
			Printf.sprintf "%s: %d unused hunks" git_code
			  unused_tokens in
		      front :: (loop git_code rest)
		    end in
            loop "" data in
	  Printf.fprintf sp_file "/*\n%s\n*/\n\n" (String.concat "\n" comment);
	  Printf.fprintf sp_file "%s" (printer_sp !ct change))
      data in
  pcm multidir_table

(* -------------------------------------------------------------------- *)
(* Entry point *)
	
let make_files (change_result,filtered_results) evolutions =
  let all_name = "_"^(!Config.file) in
  let sp_file =
    if !Config.print_sp
    then
      open_out
	(Printf.sprintf "%s/all%s.cocci" !Config.out_dir
	   (Filename.basename all_name))
    else open_out "/dev/null" in
  let get_files =
    if !Config.print_sp && !Config.git && not !Config.gitpatch
    then
      begin
	let o =
	  open_out
	    (Printf.sprintf "%s/all%s.make" !Config.out_dir
	       (Filename.basename all_name)) in
	Printf.fprintf o "DEST ?= %s\n" !Config.dest_dir;
	o
      end
    else open_out "/dev/null" in
  cocci_prolog sp_file;
  (* filtered results only *)
  List.iter
    (function (label,change_table,change_result) ->
      file_data sp_file get_files CE.ce2sp
	pre_print_to_get_files print_to_get_files
	change_result)
    filtered_results;
  close_out sp_file;
  close_out get_files

