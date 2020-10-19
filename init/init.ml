(* Entry point *)
(* processing patch files *)
let parse_code_and_perform_diff (version, dirname, filename)
  start_line current_region m p =
                 (changelist,(version, dirname, filename, current_region)))
  lines 
  |> List.map (Parse_patch.process_file parse_code_and_perform_diff) 
  |> List.concat