let mega _ = Config.same_threshold := 100

let setup_git gitdir =
  if Sys.is_directory gitdir
  then
    begin
      Config.git := true;
      Config.gitdir := gitdir
    end
  else
    begin
      Config.gitpatch := true;
      Config.file := gitdir
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
    [from;upto] -> Config.file := Printf.sprintf "%s..%s" from upto
  | _ -> failwith "bad git file"

let set_out_dir s = Config.out_dir := s

let speclist = Arg.align
 ["--git",   Arg.String setup_git, "  use a git directory";
  "--giturl", Arg.String Config.set_git_url, "  url of git repository";
  "--destdir", Arg.Set_string Config.dest_dir, "  destination of files";
  "--patch", Arg.Clear Config.git, "  use a patch file";
  "--next", Arg.String setup_next, "  use most recent tag in linux-next";

  "--min",   Arg.Set_int Config.same_threshold, "set same_threshold";
  "--minf",  Arg.Set_int Config.file_threshold, "set file_threshold";
  "--mega",  Arg.Unit mega, "set same_threshold to 100";

  "--notex",  Arg.Set Config.notex, "   no latex output";
  "--noev",  Arg.Set Config.noev, "   no evolutions";
  "--noall", Arg.Set Config.noall, "   only specialized changes";
  "--nofilters", Arg.Set Config.nofilters, "   no specialized changes";
  "--constants_only", Arg.Unit Config.set_constants_only, "   constants only";
  "--anything", Arg.Unit Config.set_anything, "   all results";

  "--verbose", Arg.Set Config.verbose, "   print all equivalence classes";

  "--print-parsable", Arg.Set Config.print_parsable,
   "   print parsable changes on stdout";
  "--print-sp", Arg.Set Config.print_sp, "   print semantic patch";
  "--out-dir", Arg.String set_out_dir, "     <dirname> specify output directory"
 ]
  

let usage = "Usage: patchparse [--git file] [--patch file], etc"

let anonymous str = Config.file := str

let main _ =
  Arg.parse speclist anonymous usage;
  (* collect lines from the git/patch file *)
  let patch_data =
    if !Config.git
    then Git_reader.git !Config.file
    else Git_reader.patch !Config.file in
  (* parse and collect differences in the lines from the git/patch file *)
  let changelists = Init.process_all_files patch_data in
  (* filter the changelists *)
  let changelists = Select_diffs.select_diffs changelists in
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

  Mktex.make_files (change_result,filtered_category_results) evolutions

let _ = main ()
