(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let setup_git gitdir =
  if Sys.is_directory gitdir
  then begin
      Config.git := true;
      Config.gitdir := gitdir
  end else begin
      Config.gitpatch := true;
      Config.file := gitdir;
      Config.outfile := gitdir
  end

let setup_next gitdir =
  Config.gitdir := gitdir;
  let lasttwo =
    Aux.cmd_to_list
      (Printf.sprintf "cd %s ; /usr/bin/git tag -l \"next*\" | tail -2"
	 gitdir) in
  Config.git := true;
  Config.noev := true;
  Config.noall := true;
  Config.print_sp := true;
  match lasttwo with
    [from;upto] ->
      Config.file := Printf.sprintf "%s..%s" from upto;
      Config.outfile := !Config.file
  | _ -> failwith "bad git file"

(* to be used with --git argument *)
let setup_days n =
  Config.git := true;
  Config.noev := true;
  Config.noall := true;
  Config.print_sp := true;
  Config.file := Printf.sprintf "--since=\"%s days ago\"" n;
  let date = List.hd (Aux.cmd_to_list (Printf.sprintf "date +%%m.%%d.%%y")) in
  Config.outfile := Printf.sprintf "%s_%s_days_ago" date n

(*****************************************************************************)
(* Options *)
(*****************************************************************************)

let speclist = Arg.align [
   "--git",   Arg.String setup_git, 
   "  use a git patch or directory";
   "--patch", Arg.Clear Config.git, 
   "  use a patch file";

   "--gitcommitlist", Arg.Set Config.gitcommitlist,
   "  interpret file as list of relevant commits";
   "--days", Arg.String setup_days, 
   "  use patches since n days ago";

   "--gitdir", Arg.Set_string Config.gitdir, 
   "  set git dir";
   "--giturl", Arg.Set_string Config.url, 
   "  url of git repository";

   "--restrict", Arg.Set_string Config.git_restrict,
   "  restrict patches obtained from git to the given directory";

   "--destdir", Arg.Set_string Config.dest_dir, 
   "  destination of files";
   "--next", Arg.String setup_next, 
   "  use most recent tag in linux-next";

   "--min",   Arg.Set_int Config.same_threshold, 
   "set same_threshold";
   "--minf",  Arg.Set_int Config.file_threshold, 
   "set file_threshold";
   "--mega",  Arg.Unit (fun () -> Config.same_threshold := 100), 
   "set same_threshold to 100";

   "--notex",  Arg.Set Config.notex, "   no latex output";
   "--noev",  Arg.Set Config.noev, "   no evolutions";
   "--noall", Arg.Set Config.noall, "   only specialized changes";
   "--nofilters", Arg.Set Config.nofilters, "   no specialized changes";
   "--constants_only", Arg.Unit Config.set_constants_only, "   constants only";
   "--anything", Arg.Unit Config.set_anything, "   all results";

   "--verbose", Arg.Set Config.verbose, "   print all equivalence classes";

   "--print-parsable", Arg.Set Config.print_parsable,
   "   print parsable changes on stdout";
   "--print-sp", Arg.Set Config.print_sp, 
   "   print semantic patch";
   "--out-dir", Arg.Set_string Config.out_dir,
   "   <dirname> specify output directory"
 ]
  

let usage = "Usage: patchparse [--git dir] [--patch file] <file> ..."

let anonymous str = Config.file := str

(*****************************************************************************)
(* entry point *)
(*****************************************************************************)

let main _ =
  Gc.set {(Gc.get ()) with Gc.stack_limit = 1000 * 1024 * 1024};

  Arg.parse speclist anonymous usage;

  (* collect lines from the git/patch file *)
  let patch_data =
    if !Config.git
    then Git_reader.git !Config.file
    else Git_reader.patch !Config.file 
  in

  (* parse and collect differences in the lines from the git/patch file *)
  let changelists = Init.process_all_files patch_data in

  (* filter the changelists *)
  let changelists = Select_diffs.select_diffs changelists in
  Printf.eprintf "changelists: %d\n" (List.length changelists);

  (* convert the changelist to a table for further processing *)
  List.iter
    (function (changelist,info) -> Prepare_eq.eqworklists changelist info)
    changelists;

  let do_evolutions = not (!Config.noev || !Config.noall) in

  let ((big_change_table,change_result),filtered_category_results) =
    Prepare_eq.eqclasses do_evolutions in
  Printf.printf "done with questions\n"; flush stdout;

  let evolutions =
    if do_evolutions
    then
      let res = Collect_evolutions4.collect big_change_table in
      (* otherwise write and clear in questions.ml *)
      Hashtbl.clear big_change_table;
      res
    else [] in
  Printf.printf "done with evolutions\n"; flush stdout;

  Mktex.make_files (change_result,filtered_category_results) evolutions;
  if !Config.print_sp
  then Mksp.make_files (change_result,filtered_category_results) evolutions

let _ = 
  main ()
