module Config = Globals

let logger = Logging.get_logger [__MODULE__]


(* Get things started.  Manage the various versions, collect the data. *)

(* more than 1 line all starting with * are comments *)
let star_threshold = 1
(* how far a star can be indented to be considered part of a comment *)
let star_near_the_beginning = 3
let boring_strings = [""; "return;"; "return0;"; "return1;"; "break;"; 
               "inti;"; "else{"; "}else{"]

let start_string s1 s2 =
  try s1 = String.sub s2 0 (String.length s1)
  with Invalid_argument _ -> false

let nstart_string n s1 s2 =
  try s1 = String.sub s2 n (String.length s1)
  with Invalid_argument _ -> false


(* -------------------------------------------------------------------- *)
(* find the start of a relevant patch and return the directory *)

let file_indicator = "+++ "

let rec split = function
    ([],n) -> ([],[])
  | ([x],n) -> ([],[x])
  | (l,0) -> ([],l)
  | (x::l,n) ->
      let (front,rest) = split (l,n-1) in
      (x::front,rest)

let driver_directory ln =
  let start = String.length file_indicator in
  let ender =
    let sp =
      try String.index_from ln start ' ' with Not_found -> String.length ln in
    let tb =
      try String.index_from ln start '\t' with Not_found -> String.length ln in
    min sp tb in
  let str = String.sub ln start (ender - start) in
  let exploded = Str.split (Str.regexp "/") str in
  let base = Filename.basename str in
  let (front,file) = split (exploded, !Config.name_depth) in
  (String.concat "/" front(*path*),
   String.concat "/" (file@[base])(*file*))

let rec find_driver_diff = function
    [] -> None
  | (num,ln)::rest ->
      if start_string file_indicator ln
      then
    let (path,file) as dd = driver_directory ln in
    let last =
      List.hd (List.rev (Str.split (Str.regexp_string ".") file)) in
    if List.mem last ["c";"java";"cpp";"py";"h"]
    then Some (dd,rest) else find_driver_diff rest
      else find_driver_diff rest

(* -------------------------------------------------------------------- *)

let parse (version : Patch.id) s =
  (* put the __line at the beginning of the line *)
  let s =
    Str.global_replace
      (Str.regexp "\\([^\n]*\\)___line=\\([0-9]+\\)")  "___line=\\2 \\1" s in

  try
    let lexbuf = Lexing.from_string s in
    Clexer.init (true, "", "", 0, 0, stderr, s);
    (*Clexer.set_current_line line_number;*)
    let parsed = Cparser2.interpret Clexer.initial lexbuf in
    Some (No_modifs.drop_outer (Ast0_to_ast.convert parsed))
  with
  | Failure x ->
      let (Patch.Id version) = version in
      let ver =
        try Hashtbl.find Config.version_table version
        with Not_found -> string_of_int version
      in
      let ver = if String.length ver < 12 then ver else String.sub ver 0 12 in
      logger#error "%s: %s\n   %s\n" ver x s;
      (*Printexc.print_backtrace stdout;*)
      None
  | Parsing.Parse_error -> 
      logger#error "Parse error";
      None
(*  | x -> None*)


(* -------------------------------------------------------------------- *)
(* preprocessing of the patch lines *)

(*subtil: important to put space before ___ otherwise other parsing
   functions of Init may be confused (such as driver_directory who may
   return as the filename of the driver konita.c___line=123 which may in
   turn cause pb when we need to get cinfo from such a file the pb is only
   visible in certain patch, such as 2.6.13 and with using -use_info_stat
   because the patch format is +++ b/sound/pci/trident/trident.c whereas in
   old version it is +++ b/arch/cris/arch-v10/drivers/serial.c Thu Jul 10
   13:16:28 2003

*)

let integrate_line_number n ln = ln ^ "  ___line=" ^ string_of_int n

(* Patch seems to use \ for informational messages. *)
let rec drop_slash_input lines =
  List.filter
    (function (patch_line,ln) ->
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
    lines

(* -------------------------------------------------------------------- *)
(* collecting a changed region *)

let drop_initial_spaces all_start_with_star seen_star s =
  let s = String.concat "" (Str.split (Str.regexp "\n") s) in
  let contains_at s = (* typical of comments *)
    match Str.split_delim (Str.regexp "@") s with
      _::_::_ -> true
    | _ -> false in
  let pieces = Str.split_delim (Str.regexp_string "++++") s in
  if pieces = []
  then (s,false,false)
  else
    let prefix =
      String.concat "++++" (List.rev(List.tl(List.rev pieces))) ^ "++++" in
    let s = List.hd (List.rev pieces) in
    (* returns string tail + indication of * at start*)
    let len = String.length s in
    let rec loop n =
      if n = len
      then ("",true,seen_star)
      else
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
    let (sres,flag,seen_star) = loop 0 in
    if sres = ""
    then (sres,flag,seen_star)
    else (prefix^" "^s,flag,seen_star)


let minus_line_token = "++++minus_line++++"
let plus_line_token = "++++plus_line++++"
let context_line_token = "++++context_line++++"
let collect_lines bounded str lines =
  let drop_control ln = (* drop +/- *)
    let len = String.length ln in
    (String.get ln 0,
     len > 1 && (List.mem (String.get ln 1) [' ';'\t']),
     String.sub ln 1 (len - 1)) in
  let rec collect_conforming bounded (region,len) lines =
    if bounded && len > Config.length_threshold
    then
      let rec drop = function
      [] -> ([],len,[])
    | ((n,ln)::xs) as lines ->
        if start_string str ln
        then drop xs
        else ([],len,lines) in
      drop lines
    else
      match lines with
    [] -> (List.rev region,len,[])
      | ((n,ln)::rest) as lines ->
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
  let (region,len,after) = collect_conforming bounded ([],0) lines in
  if bounded && len > Config.length_threshold
  then ("",after)
  else
    let rec collect_non_comments ctr collected
    seen_comment_starter in_all_start_with_star seen_star =
      function
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
    match collect_non_comments 1 "" false true false region with
      (_,true) when List.length region > star_threshold (*just comments*) ->
    ("",after)
    | (collected,_) -> (collected,after)

(* -------------------------------------------------------------------- *)
(* process the - and + lines until reaching an occurrence of diff *)

let all_blank s =
  let all_blank = ref true in
  String.iter
    (function ' ' | '\n' | '\t' -> () | _ -> all_blank := false)
    s;
  !all_blank


(* PAD: *)
let _compteur = ref 0
let _current_function = ref None
let current_region () = 
  match !_current_function with
  | None -> Patch.Region (!_compteur)
  (* try get name of func, not just split in region,  otherwise will be split
     wheras in fact in same function *)
  | Some s -> Patch.InFunction s (* TODO ? get also compteur,  so sure that
   dont mix together same functions  defined in different files *)

(* focus on ; -- drop code between ;s that has no minus or plus code *)
let focus s marker =
  let pieces = Str.split (Str.regexp ";[ \t]*\n") s in
  let marker = Str.regexp marker in
  let marker2 = Str.regexp context_line_token in
  let pieces =
    List.filter
      (function s ->
    match Str.split_delim marker s with
      _::_::_ -> true
    | _ ->
        match Str.split_delim marker2 s with
          _::_::_ -> false
        | _ -> true)
      pieces in
  String.concat ";" pieces

(* processes a single file *)
let process_lines version dirname filename lines =
  let block_end start_line before minus plus after =
    if not (all_blank minus && all_blank plus)
    then
      begin
    let m = focus (before^minus^after) minus_line_token in
    let p = focus (before^plus^after) plus_line_token in

    Parse_error.start_line := start_line;

      (* ---------------- THIS IS THE IMPORTANT PLACE --------------- *)
    match (parse version m, parse version p) with
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
      end
    else [] in
  let rec loop start_count start_line before = function
      (* start count is the number of the region
     start line is the number of the first line in this region *)
      [] -> ([],[])
    | ((n,ln)::rest) as lines ->
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
        
  match lines with
    [] -> ([],[])
  | (start_line,ln)::rest ->
      let (collected,after) = collect_lines false " " lines in
      loop 1 start_line collected after

(* -------------------------------------------------------------------- *)
(* processing patch files *)

(* collect all the data for a single patch file, ie a single version *)
let process_file (version,lines) =
  let lines = drop_slash_input lines in
  let rec loop lines =
    match find_driver_diff lines with
      None -> []
    | Some ((path,file),rest) ->
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
  in loop lines

let process_all_files (lines : Patch.t list) =
  logger#info "in process all files";
  List.concat (List.map process_file lines)
