open Common
(* pad: I've renamed config.ml to globals.ml to avoid conflict with
 * compiler-libs/config.ml which is used when linking with pfff commons,
 * but I restore its original name here to look as before.
*)
module Config = Globals

let logger = Logging.get_logger [__MODULE__]

let log_config_file = ref "log_config.json"

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Parse patches and extract recurring transformations.
 * It also classify those transformations in a taxonomy.
 * 
 * The original goal of patchparse was to study collateral evolutions in
 * the Linux kernel, as documented in the Eurosys'06 paper:
 *  "Understanding Collateral Evolution in Linux Device Drivers".
 * The PDF of the paper is in the docs/ directory, and reading
 * section 5.2 is especially useful to understand patchparse.
 *)


(* -------------------------------------------------------------------- *)
(*****************************************************************************)
(* Local flags *)
(*****************************************************************************)

(* pad: action mode, used for all the -dump_xxx actions. Use commons/ lib *)
let action = ref ""

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
(* Actions *)
(*****************************************************************************)

let dump_patch file =
  let xs = Patch_reader.patch file in

  xs |> List.iter (fun patch -> 
    pr2 (Patch.show patch);
  )

let dump_hunk file =
  let xs = Patch_reader.patch file in
  xs |> List.iter (fun patch -> 
    let hunks = Parse_patch.hunks_of_patch patch in

    hunks |> List.iter (fun x -> pr (Patch.show_hunk x));
  ) |> ignore

let dump_ast0 file =
  let xs = Patch_reader.patch file in
  xs |> List.iter (fun patch -> 
    let hunks = Parse_patch.hunks_of_patch patch in
    hunks |> List.iter (fun x -> 
     pr "Minus:";
     let ast0 = Parse_code.parse_ast0 x.Patch.patch_id x.minus in
     pr (Ast0.show_codelist ast0);
     pr "Plus:";
     let ast0 = Parse_code.parse_ast0 x.Patch.patch_id x.plus in
     pr (Ast0.show_codelist ast0);
   )
  ) |> ignore

let dump_ast file =
  let xs = Patch_reader.patch file in
  xs |> List.iter (fun patch -> 
    let hunks = Parse_patch.hunks_of_patch patch in
    hunks |> List.iter (fun x -> 
     pr "Minus:";
     (match Parse_code.parse x.Patch.patch_id x.minus with
     | None -> failwith "no ast"
     | Some xxs -> xxs |> List.iter (fun xs -> pr (Ast.show_codelist xs)));
     pr "Plus:";
     (match Parse_code.parse x.Patch.patch_id x.plus with
     | None -> failwith "no ast"
     | Some xxs -> xxs |> List.iter (fun xs -> pr (Ast.show_codelist xs)));

    )
  ) |> ignore


let dump_ce file =
  let patch_data = Patch_reader.patch file in
  let changelists = Init.process_all_files patch_data in

  changelists |> List.iter (fun (xs, origin) -> 
    pr (Change_tree.show_origin origin);
    xs |> List.iter (function
    | Change_tree.CC (ce, _cg) -> 
       pr (Ce.show_ce ce);
    | _ -> raise Impossible
    );
  )

let dump_changelist file =
  let patch_data = Patch_reader.patch file in
  let changelists = Init.process_all_files patch_data in

  changelists |> List.iter (fun x -> 
    pr2 (Change_tree.show_changelist x);
  )

let dump_worklist file = 
  let patch_data = Patch_reader.patch file in
  let changelists = Init.process_all_files patch_data in
  changelists |> List.iter Prepare_eq.eqworklists;

  let worklist = Prepare_eq.gsemi__change_worklist in
  worklist |> Hashtbl.iter (fun k v ->
      pr "KEY:";
      pr (spf "%d" k);
      pr "VALUE:";
      pr (Eq_classes.show_workset v);
  )

let dump_changetable file = 
  let patch_data = Patch_reader.patch file in
  let changelists = Init.process_all_files patch_data in

  Config.same_threshold := 1;
  Config.file_threshold := 1;
  Config.version_threshold := 0;
  Config.directory_threshold := 0;

  changelists |> List.iter Prepare_eq.eqworklists;
  let ((big_change_table,change_result),filtered_category_results) =
    Prepare_eq.eqclasses true in
  (("all", big_change_table, change_result)::filtered_category_results) 
  |> List.iter (fun (categ, changetable, result) ->
      pr (spf "CATEGORY: %s" categ);
      pr (spf "CHANGETABLE");
      changetable |> Hashtbl.iter (fun k xs ->
          pr "KEY:";
          pr (Ce.show_ce k);
          pr "VALUE:";
          xs |> List.iter (fun v -> pr (Eq_classes.show_changes v));
      );
      pr (spf "QUESTIONS");
      pr (Questions.show_result result);
  )

let dump_evolution file = 
  let patch_data = Patch_reader.patch file in
  let changelists = Init.process_all_files patch_data in

  Config.same_threshold := 1;
  Config.file_threshold := 1;
  Config.version_threshold := 0;
  Config.directory_threshold := 0;

  changelists |> List.iter Prepare_eq.eqworklists;
  let ((big_change_table,_change_result),_filtered_category_results) =
    Prepare_eq.eqclasses true in
  let (_evolutions : Evolution.t list) =
    Collect_evolutions4.collect big_change_table in
  ()
(*
  evolutions |> List.iter (fun evo ->
     pr (Evolution.show evo)
  )
*)  
 

let actions () = [
  "-dump_patch", " <file>",
  Common.mk_action_1_arg dump_patch;
  "-dump_hunk", " <file>",
  Common.mk_action_1_arg dump_hunk;

  "-dump_ast0", " <file>",
  Common.mk_action_1_arg dump_ast0;
  "-dump_ast", " <file>",
  Common.mk_action_1_arg dump_ast;

  "-dump_ce", " <file>",
  Common.mk_action_1_arg dump_ce;
  "-dump_cc", " <file>",
  Common.mk_action_1_arg dump_changelist;

  "-dump_ct", " <file>",
  Common.mk_action_1_arg dump_changetable;
  "-dump_evo", " <file>",
  Common.mk_action_1_arg dump_evolution;

  (* aliases, longer form *)
  "-dump_changeelement", " <file>",
  Common.mk_action_1_arg dump_ce;
  "-dump_diff", " <file>",
  Common.mk_action_1_arg dump_changelist;
  "-dump_changelist", " <file>",
  Common.mk_action_1_arg dump_changelist;
  "-dump_worklist", " <file>",
  Common.mk_action_1_arg dump_worklist;
  "-dump_changetable", " <file>",
  Common.mk_action_1_arg dump_changetable;
  "-dump_evolution", " <file>",
  Common.mk_action_1_arg dump_evolution;
 ]

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
    "   <dirname> specify output directory";

    "--log_config_file", Arg.Set_string log_config_file,
    "  <file> logging configuration";
  ]


let usage = "Usage: patchparse [--git dir] [--patch file] <file> ..."

let options () = 
  speclist @
  Common.options_of_actions action (actions())

(*****************************************************************************)
(* entry point *)
(*****************************************************************************)

let main _ =
  Gc.set {(Gc.get ()) with Gc.stack_limit = 1000 * 1024 * 1024};

  (* pad: modifies many of the Config.xxx globals *)
  (* pad: old: Arg.parse speclist (fun str -> Config.file := str)  usage; *)
  let args = Common.parse_options (options()) usage Sys.argv in

  if Sys.file_exists !log_config_file
  then begin
    Logging.load_config_file !log_config_file;
    logger#info "loaded %s" !log_config_file;
  end;
  
 (match args with

 (* --------------------------------------------------------- *)
 (* actions, useful to debug subpart *)
 (* --------------------------------------------------------- *)
 | xs when List.mem !action (Common.action_list (actions())) -> 
        Common.do_action !action xs (actions())

 | _ when not (Common.null_string !action) -> 
        failwith ("unrecognized action or wrong params: " ^ !action)

 (* --------------------------------------------------------- *)
 (* main entry *)
 (* --------------------------------------------------------- *)

 | [x] ->
   Config.file := x;

  (* collect lines from the git/patch file *)
  let patch_data =
    if !Config.git
    (* uses many of the globals in Config (e.g., Config.gitcommitlist) *)
    then Git_reader.git !Config.file
    else Patch_reader.patch !Config.file 
  in

  (* parse and collect differences in the lines from the git/patch file *)
  let changelists = Init.process_all_files patch_data in
  logger#info "done with init";

  (* filter the changelists *)
  let changelists = Select_diffs.select_diffs changelists in
  logger#info "changelists: %d" (List.length changelists);

  (* convert the changelist to a table for further processing *)
  changelists |> List.iter Prepare_eq.eqworklists;

  let do_evolutions = not (!Config.noev || !Config.noall) in

  let ((big_change_table,change_result),filtered_category_results) =
    Prepare_eq.eqclasses do_evolutions in
  logger#info "done with questions";

  let (evolutions : Evolution.t list) =
    if do_evolutions
    then
      let res = Collect_evolutions4.collect big_change_table in
      (* otherwise write and clear in questions.ml *)
      Hashtbl.clear big_change_table;
      res
    else [] 
  in
  logger#info "done with evolutions";

  Mktex.make_files (change_result,filtered_category_results) evolutions;
  if !Config.print_sp
  then Mksp.make_files (change_result,filtered_category_results) evolutions

 (* --------------------------------------------------------- *)
 (* empty entry *)
 (* --------------------------------------------------------- *)
 | _ -> 
    Common.usage usage (options());
    failwith "too few or too many arguments"
 )

let _ = 
  Common.main_boilerplate (fun () -> 
    main ()
  )
