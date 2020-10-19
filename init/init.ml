let logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* processing patch files *)

let parse_code_and_perform_diff (version, dirname, filename)
  start_line current_region m p =
        Parse_error.start_line := start_line;

        (* ---------------- THIS IS THE IMPORTANT PLACE --------------- *)
        match (Parse_code.parse version m, Parse_code.parse version p) with
        | (Some mres,Some pres) ->
          (try
             let changelists = Diff.diff mres pres in
             changelists |> List.map (function changelist ->
                 (changelist,(version, dirname, filename, current_region)))
           with Failure s ->
             logger#error "%d: failed on %s:\n---\n%s\n+++\n%s\n\n"
               start_line s m p;
             []
          )
        | _ -> []

let process_all_files (lines : Patch.t list) =
  logger#info "in process all files";
  lines 
  |> List.map (Parse_patch.process_file parse_code_and_perform_diff) 
  |> List.concat
