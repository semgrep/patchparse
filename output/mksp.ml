module CE = Ce

(* -------------------------------------------------------------------- *)
(* Print the data in the VERSION or DIRECTORY cases *)

let split_git_version version =
  match Str.split (Str.regexp " ") version with
    git_code :: rest -> let rest = String.concat " " rest in (git_code,rest)
  | _ -> failwith "bad version"

(* -------------------------------------------------------------------- *)
(* Semantic patch stuff *)
	
let cocci_prolog o rules =
  Printf.fprintf o
    "virtual invalid\nvirtual opportunities\nvirtual select\nvirtual prequel\n\n";
  let ct = ref 0 in
  List.iter
    (function (label,change_table,change_result) ->
      let (_,_,multidir_table,_,_) = change_result in
      List.iter
	(function _ ->
	  ct := !ct + 1;
	  Printf.fprintf o "virtual select_rule%d\n" !ct)
	multidir_table)
    rules;
  if !ct > 0 then Printf.fprintf o "\n"

let ct = ref 0 (* semantic patch counter *)

(* -------------------------------------------------------------------- *)
(* Make file stuff *)
	
let pre_print_to_get_files o ct =
  if !Config.git && not (!Config.gitdir = "")
  then
    begin
      Printf.fprintf o "\nrule%d:\n" ct;
      Printf.fprintf o "\t/bin/mkdir -p $(OUT)/rule%d\n" ct;
      Printf.fprintf o "\t/bin/rm -f $(OUT)/rule%d/index\n" ct;
      Printf.fprintf o "\t/usr/bin/touch $(OUT)/rule%d/index\n" ct;
      Printf.fprintf o "\t/bin/rm -f $(OUT)/rule%d/redodiff\n" ct;
      Printf.fprintf o "\t/usr/bin/touch $(OUT)/rule%d/redodiff\n" ct
    end

let mkname file =
  String.concat "__" (Str.split (Str.regexp "/") file)

let print_to_get_files o ct code =
  if !Config.git && not (!Config.gitdir = "")
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
      List.iter
	(function file ->
	  Printf.fprintf o "\tcd %s ; \\\n" !Config.gitdir;
	  let resfile s =
	    match List.rev (Str.split (Str.regexp_string ".") s) with
	      x::xs -> String.concat "." (List.rev (x :: "res" :: xs))
	    | _ -> failwith "bad file" in
	  let cocciresfile s = s ^ ".cocci_res" in
	  Printf.fprintf o
	    "\tgit cat-file blob %s^:%s > $(OUT)/rule%d/%s ; \\\n\tgit cat-file blob %s:%s > $(OUT)/rule%d/%s ; \\\n"
	    code file ct (mkname file)
	    code file ct (resfile(mkname file));
	  Printf.fprintf o "\techo %s %s >> $(OUT)/rule%d/index\n"
	    (mkname file) (resfile(mkname file)) ct;
	  Printf.fprintf o
	    "\techo test -e rule%d/%s \\&\\& (diff -u rule%d/%s rule%d/%s \\| diffstat) >> $(OUT)/rule%d/redodiff\n"
	    ct (cocciresfile(mkname file)) ct (resfile(mkname file))
	    ct (cocciresfile(mkname file)) ct)
	files;
      List.length files
    end
  else 0

let run_spdiff cocci o rules =
(*  Printf.fprintf o
    "\nSCMD=spdiff -specfile index -spatch -prune -filter_spatches -only_changes -threshold\n\n"; *)
  Printf.fprintf o
    "\nSCMD=spdiff -specfile index -prune -filter_spatches -only_changes -threshold\n\n";
  let ct = ref 0 in
  List.iter
    (function (label,change_table,change_result) ->
      let (_,_,multidir_table,_,_) = change_result in
      List.iter
	(function _ ->
	  ct := !ct + 1;
	  Printf.fprintf o "rule%d.spd: rule%d\n" !ct !ct;
	  Printf.fprintf o
	    "\tcd $(OUT)/rule%d; $(SCMD) $(FILES%d) > $(OUT)/rule%d/spdiff.out 2> $(OUT)/rule%d/spdiff.tmp\n\n"
	    !ct !ct !ct !ct)
	multidir_table)
    rules;
  Printf.fprintf o "spdiffall: %s\n"
    (String.concat " "
       (let rec loop = function
	   0 -> []
	 | n -> (Printf.sprintf "rule%d.spd" n) :: (loop (n-1)) in
       List.rev (loop !ct)))

let cocci_header cocci o =
  Printf.fprintf o
    "\nCMD=spatch.opt -quiet -timeout 120 -dir %s -use_glimpse \\\n-cocci_file %s.cocci -D select\n"
    !Config.gitdir cocci;
  Printf.fprintf o
    "\nREDOCMD=spatch.opt -include-headers -quiet -out-place -timeout 120 \\\n-cocci_file %s.cocci -D select\n"
    cocci;
  Printf.fprintf o
    "\nPCMD=~/prequel/implem2/prequel --git %s \\\n--sp %s.cocci --commits v3.0.. --pct 0 --cores 24 \\\n--all-lines \\\n--cocciargs \"--very-quiet -D select -D invalid -D prequel --no-includes\"\n\n"
    !Config.gitdir cocci

let run_coccis cocci o rules =
  let ct = ref 0 in
  List.iter
    (function (label,change_table,change_result) ->
      let (_,_,multidir_table,_,_) = change_result in
      List.iter
	(function _ ->
	  ct := !ct + 1;
	  Printf.fprintf o "rule%d.out:\n" !ct;
	  Printf.fprintf o "\tmkdir -p $(OUT)\n";
	  Printf.fprintf o
	    "\tgrep invalid %s.cocci | grep -q rule%d || \\\n" cocci !ct;
	  Printf.fprintf o
	    "\t$(CMD) -D select_rule%d > $(OUT)/rule%d.out 2> $(OUT)/rule%d.tmp\n\n"
	    !ct !ct !ct)
	multidir_table)
    rules;
  Printf.fprintf o "vrunall: %s\n\n"
    (String.concat " "
       (let rec loop = function
	   0 -> []
	 | n -> (Printf.sprintf "rule%d.out" n) :: (loop (n-1)) in
       List.rev (loop !ct)))

(* include invalids *)
let run_icoccis cocci o rules =
  let ct = ref 0 in
  List.iter
    (function (label,change_table,change_result) ->
      let (_,_,multidir_table,_,_) = change_result in
      List.iter
	(function _ ->
	  ct := !ct + 1;
	  Printf.fprintf o "irule%d.out:\n" !ct;
	  Printf.fprintf o "\tmkdir -p $(OUT)\n";
	  Printf.fprintf o
	    "\t$(CMD) -D invalid -D select_rule%d > $(OUT)/rule%d.iout 2> $(OUT)/rule%d.itmp\n\n"
	    !ct !ct !ct)
	multidir_table)
    rules;
  Printf.fprintf o "irunall: %s\n\n"
    (String.concat " "
       (let rec loop = function
	   0 -> []
	 | n -> (Printf.sprintf "irule%d.out" n) :: (loop (n-1)) in
       List.rev (loop !ct)))

let rerun_coccis cocci o rules =
  (* run patchparse inferred rule on old patches *)
  let ct = ref 0 in
  List.iter
    (function (label,change_table,change_result) ->
      let (_,_,multidir_table,_,_) = change_result in
      List.iter
	(function _ ->
	  ct := !ct + 1;
	  Printf.fprintf o "redorule%d.out: rule%d\n" !ct !ct;
	  Printf.fprintf o "\tmkdir -p $(OUT)\n";
	  Printf.fprintf o
	    "\tgrep invalid %s.cocci | grep -q \"rule%d \" || \\\n" cocci !ct;
	  Printf.fprintf o
	    "\t($(REDOCMD) -dir rule%d -D select_rule%d > $(OUT)/rule%d.rout "
	    !ct !ct !ct;
	  Printf.fprintf o "2> $(OUT)/rule%d.rtmp && \\\n" !ct;
	  Printf.fprintf o "\tsh rule%d/redodiff > $(OUT)/rule%d.rdiff)\n\n"
	    !ct !ct)
	multidir_table)
    rules;
  Printf.fprintf o "redorunall: %s\n\n"
    (String.concat " "
       (let rec loop = function
	   0 -> []
	 | n -> (Printf.sprintf "redorule%d.out" n) :: (loop (n-1)) in
       List.rev (loop !ct)))

(* include invalids *)
let rerun_icoccis cocci o rules =
  (* run patchparse inferred rule on old patches *)
  let ct = ref 0 in
  List.iter
    (function (label,change_table,change_result) ->
      let (_,_,multidir_table,_,_) = change_result in
      List.iter
	(function _ ->
	  ct := !ct + 1;
	  Printf.fprintf o "iredorule%d.out: rule%d\n" !ct !ct;
	  Printf.fprintf o "\tmkdir -p $(OUT)\n";
	  Printf.fprintf o
	    "\t$(REDOCMD) -dir rule%d -D invalid -D select_rule%d "
	    !ct !ct;
	  Printf.fprintf o "> $(OUT)/rule%d.irout 2> $(OUT)/rule%d.irtmp\n"
	    !ct !ct;
	  Printf.fprintf o "\tsh rule%d/redodiff > $(OUT)/rule%d.irdiff\n\n"
	    !ct !ct)
	multidir_table)
    rules;
  Printf.fprintf o "iredorunall: %s\n\n"
    (String.concat " "
       (let rec loop = function
	   0 -> []
	 | n -> (Printf.sprintf "iredorule%d.out" n) :: (loop (n-1)) in
       List.rev (loop !ct)))
  
(* run prequel *)
let run_pcoccis cocci o rules =
  let ct = ref 0 in
  List.iter
    (function (label,change_table,change_result) ->
      let (_,_,multidir_table,_,_) = change_result in
      List.iter
	(function _ ->
	  ct := !ct + 1;
	  Printf.fprintf o "prule%d.out:\n" !ct;
	  Printf.fprintf o "\tmkdir -p $(OUT)\n";
	  Printf.fprintf o
	    "\t$(PCMD) --cocciargs \"-D select_rule%d\" > $(OUT)/rule%d.pout 2> $(OUT)/rule%d.ptmp\n\n"
	    !ct !ct !ct)
	multidir_table)
    rules;
  Printf.fprintf o "prunall: %s\n\n"
    (String.concat " "
       (let rec loop = function
	   0 -> []
	 | n -> (Printf.sprintf "prule%d.out" n) :: (loop (n-1)) in
       List.rev (loop !ct)))

(* count opportunities *)
let run_ococcis cocci o rules =
  let ct = ref 0 in
  List.iter
    (function (label,change_table,change_result) ->
      let (_,_,multidir_table,_,_) = change_result in
      List.iter
	(function _ ->
	  ct := !ct + 1;
	  Printf.fprintf o "orule%d.out:\n" !ct;
	  Printf.fprintf o "\tmkdir -p $(OUT)\n";
	  Printf.fprintf o
	    "\t$(CMD) -no_show_diff -D opportunities -D invalid -D select_rule%d > $(OUT)/rule%d.oout 2> $(OUT)/rule%d.otmp\n\n"
	    !ct !ct !ct)
	multidir_table)
    rules;
  Printf.fprintf o "orunall: %s\n\n"
    (String.concat " "
       (let rec loop = function
	   0 -> []
	 | n -> (Printf.sprintf "orule%d.out" n) :: (loop (n-1)) in
       List.rev (loop !ct)))

let cocci_tail cocci o =
  Printf.fprintf o
    "\nrunall: vrunall irunall redovrunall iredorunall orunall prunall\n\n"

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
	  let fct_comment =
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
		      let fct = print_to_get_files get_files !ct git_code in
		      let unused_tokens =
			try !(Hashtbl.find Eqclasses.version_unused_table
				version)
			with Not_found -> 0 in
		      let front =
			Printf.sprintf "%s: %d unused hunks" git_code
			  unused_tokens in
		      (fct,front) :: (loop git_code rest)
		    end in
            loop "" data in
	  let (fcts,comment) = List.split fct_comment in
	  let fct = List.fold_left (+) 0 fcts in
	  Printf.fprintf get_files "FILES%d=%d\n" !ct fct;
	  Printf.fprintf sp_file "/*\n%s\n*/\n\n" (String.concat "\n" comment);
	  Printf.fprintf sp_file "%s" (printer_sp !ct change))
      data in
  pcm multidir_table

(* -------------------------------------------------------------------- *)
(* Entry point *)
	
let make_files (change_result,filtered_results) evolutions =
  let all_name = "_"^(!Config.outfile) in
  let sp_file =
    if !Config.print_sp
    then
      open_out
	(Printf.sprintf "%s/all%s.cocci" !Config.out_dir
	   (Filename.basename all_name))
    else open_out "/dev/null" in
  let get_files =
    if !Config.print_sp && !Config.git && not (!Config.gitdir = "")
    then
      begin
	let o =
	  open_out
	    (Printf.sprintf "%s/all%s.make" !Config.out_dir
	       (Filename.basename all_name)) in
	Printf.fprintf o "DEST ?= %s\n" !Config.dest_dir;
	let dir = Sys.getcwd() in
	Printf.fprintf o "OUT = %s/%s/$(DEST)\n" dir !Config.out_dir;
	o
      end
    else open_out "/dev/null" in
  cocci_prolog sp_file filtered_results;
  (* filtered results only *)
  List.iter
    (function (label,change_table,change_result) ->
      file_data sp_file get_files CE.ce2sp
	pre_print_to_get_files print_to_get_files
	change_result)
    filtered_results;
  let cocci = "all"^(Filename.basename all_name) in
  run_spdiff cocci get_files filtered_results;
  cocci_header cocci get_files;
  run_coccis cocci get_files filtered_results;
  run_icoccis cocci get_files filtered_results;
  rerun_coccis cocci get_files filtered_results;
  rerun_icoccis cocci get_files filtered_results;
  run_ococcis cocci get_files filtered_results;
  run_pcoccis cocci get_files filtered_results;
  cocci_tail cocci get_files;
  close_out sp_file;
  close_out get_files
