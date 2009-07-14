let mega _ = Config.same_threshold := 100

let git = ref true

let set_out_dir s = Config.out_dir := s

let speclist = Arg.align
 ["-git",   Arg.Set git, "  use a git file";
  "-patch", Arg.Clear git, "  use a patch file";
  "-min",   Arg.Set_int Config.same_threshold, "set same_threshold";
  "-minf",  Arg.Set_int Config.file_threshold, "set file_threshold";
  "-mega",  Arg.Unit mega, "set same_threshold to 100";
  "-notex",  Arg.Set Config.noev, "   no latex output";
  "-noev",  Arg.Set Config.noev, "   no evolutions";
  "-noall", Arg.Set Config.noall, "   only specialized changes";
  "-nofilters", Arg.Set Config.nofilters, "   no specialized changes";
  "-constants_only", Arg.Unit Config.set_constants_only, "   constants only";
  "-anything", Arg.Unit Config.set_anything, "   all results";
  "-verbose", Arg.Set Config.verbose, "   print all equivalence classes";
  "-print_parsable", Arg.Set Config.print_parsable,
   "   print parsable changes on stdout";
  "-out_dir", Arg.String set_out_dir, "     <dirname> specify output directory"
 ]
  

let usage = "Usage: patchparse [-git file] [-patch file], etc"

let anonymous str = Config.file := str

let main _ =
  Arg.parse speclist anonymous usage;
  (* collect lines from the git/patch file *)
  let patch_data =
    if !git
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
