
let logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

                      "inti;"; "else{"; "}else{"]
    let (front,rest) = split (l,n-1) in
    (x::front,rest)
    if start_string file_indicator ln
    then
      let (path,file) as dd = driver_directory ln in
      let last =
        List.hd (List.rev (Str.split (Str.regexp_string ".") file)) in
      if List.mem last ["c";"java";"cpp";"py";"h"]
      then Some (dd,rest) else find_driver_diff rest
    else find_driver_diff rest
       if ln = ""
       then false
       else
         match String.get ln 0 with
         (* see arguments to start_string *)
           '+' | '-' | ' ' | '@' | 'd' | 'i' | 'n' | 'o' | 's' | 'r' -> true
         | '\\' | 'B' (*Binary files differ*) -> false
         (* mode, found when analyzing the django repository *)
         | 'm' -> false
         | c ->
           failwith
             (Printf.sprintf "%d: unexpected patch character %c: %s"
                patch_line c ln))
        let char = String.sub s n 1 in
        match char with
          " " | "\n" | "\t" -> loop (n+1)
        | "*"  when contains_at s -> ("",true,true)
        | "*" when all_start_with_star && n <= 1 &&
                   not (Aux.substring "*/" s) -> ("",true,true)
        | "*" when ((Aux.substring " for " s) || (Aux.substring " while " s) ||
                    (Aux.substring " if " s)) -> ("",true,true)
        | "*" when n < star_near_the_beginning ->
          (String.sub s n (String.length s - n),true,true)
        | _ -> (String.sub s n (String.length s - n),false,seen_star) in
          [] -> ([],len,[])
        | ((n,ln)::xs) as lines ->
          if start_string str ln
          then drop xs
          else ([],len,lines) in
        [] -> (List.rev region,len,[])
        if start_string str ln
        then
          let (ty,space_start,ln) = drop_control ln in
          let front =
            match ty with
              '-' -> minus_line_token
            | '+' -> plus_line_token
            | _ -> context_line_token in
          let middle =
            if space_start then "++++space++++" else "" in
          let mylen =
            String.length
              (String.concat "" (Str.split (Str.regexp "[ \t]") ln)) in
          let ln = front ^ " " ^ middle ^ " " ^ ln in
          collect_conforming bounded
            ((n,ln)::region,mylen + len)
            rest
        else (List.rev region,len,lines) in
        seen_comment_starter in_all_start_with_star seen_star =
        [] -> (collected,in_all_start_with_star)
      | (n,ln)::rest ->
        let (ln,starts_with_star,seen_star) =
          drop_initial_spaces in_all_start_with_star seen_star ln in
        let all_start_with_star = starts_with_star && in_all_start_with_star in
        let is_comment_end_line = (* returns the rest, after */ *)
          (* this is for the case where we find the end of a comment, but
             have not seen the beginning *)
          if seen_comment_starter
          then None
          else
            (try
               let comment_end = Aux.substring_index "*/" ln in
               if Aux.substring "/*" (String.sub ln 0 comment_end)
               then None (* not a comment end line *)
               else
                 Some (String.sub ln (comment_end+2)
                         ((String.length ln) - (comment_end+2)))
             with Not_found -> None) in
        let seen_comment_starter =
          seen_comment_starter || Aux.substring "/*" ln ||
          Aux.substring "//" ln in
        let (collected_so_far,all_start_with_star) =
          let ln = integrate_line_number n ln in
          match is_comment_end_line with
            Some ln -> (ln^"\n",false)
          | None ->
            if all_start_with_star = in_all_start_with_star ||
               ctr <= star_threshold || not seen_star
            then (collected^ln^"\n",all_start_with_star)
            else ("",all_start_with_star) in
        collect_non_comments (ctr+1) collected_so_far seen_comment_starter
          all_start_with_star seen_star rest in
      ("",after)
                                    dont mix together same functions  defined in different files *)
       match Str.split_delim marker s with
         _::_::_ -> true
       | _ ->
         match Str.split_delim marker2 s with
           _::_::_ -> false
         | _ -> true)
        let m = focus (before^minus^after) minus_line_token in
        let p = focus (before^plus^after) plus_line_token in

        Parse_error.start_line := start_line;

        (* ---------------- THIS IS THE IMPORTANT PLACE --------------- *)
        match (Parse_code.parse version m, Parse_code.parse version p) with
        | (Some mres,Some pres) ->
          (try
             let changelists = Diff.diff mres pres in
             changelists |> List.map (function changelist ->
                 (changelist,(version, dirname, filename, current_region())))
           with Failure s ->
             logger#error "%d: failed on %s:\n---\n%s\n+++\n%s\n\n"
               start_line s m p;
             []
          )
        | _ -> []
    (* start count is the number of the region
       start line is the number of the first line in this region *)
      if start_string "diff " ln || start_string "--- " ln ||
         start_string "index " ln ||
         start_string "similarity index " ln ||
         start_string "rename from " ln ||
         start_string "new file mode " ln ||
         start_string "old file mode " ln ||
         start_string "new mode " ln ||
         start_string "old mode " ln
      then ([],rest)
      else
      if start_string "@@ " ln
      then
        begin
          let start =
            "@@[^@]*@@\\(.*\\)[ \t]+\\([a-zA-Z_][a-zA-Z_0-9]+\\)[\t ]*(" in
          (if Str.string_match (Str.regexp start) ln 0
           then
             let functionname = Str.matched_group 2 ln in
             (*pr2 ("GOTONE" ^ functionname);*)
             _current_function := Some functionname
           else _current_function := None);
          incr _compteur;
          let (Paths.File sfilename) = filename in
          if !Config.notex
          then logger#info "%s region %d:\n\n" sfilename start_count;
          let start_line = n in
          let (collected,rest) = collect_lines false " " rest in
          loop (start_count + 1) start_line collected rest
        end
      else
        begin
          let (minus,rest) = collect_lines true "-" lines in
          let (plus,rest) = collect_lines true "+" rest in
          let new_start_line = n in
          let (after,rest) = collect_lines false " " rest in
          let cur = block_end start_line before minus plus after in
          let (rest_res,rest) =
            loop start_count new_start_line after rest in
          (cur@rest_res,rest)
        end in

    let (collected,after) = collect_lines false " " lines in
    loop 1 start_line collected after

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
      Config.complete_filename := path^"/"^file;
      let fl = Filename.basename file in
      Config.filename :=
        Aux.normalize
          (String.lowercase_ascii (Filename.chop_extension fl))(*drop .c*);
      let dir = Paths.Dir path in
      let file = Paths.File !Config.complete_filename in
      let (res,after) =
        process_lines version dir file rest in
      res@(loop after)
  logger#info "in process all files";