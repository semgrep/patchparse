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
      then Some (driver_directory ln,rest)
      else find_driver_diff rest

(* -------------------------------------------------------------------- *)

let parse s =
  (* put the __line at the beginning of the line *)
  let s =
    Str.global_replace
      (Str.regexp "\\([^\n]*\\)___line=\\([0-9]+\\)")  "___line=\\2 \\1" s in
  (* Common.pr2 s; *)

  try
    let lexbuf = Lexing.from_string s in
    Clexer.init (true, "", "", 0, 0, stderr, s);
    (*Clexer.set_current_line line_number;*)
    let parsed = Cparser2.interpret Clexer.initial lexbuf in
    Some (Ast0.convert parsed)
  with
    Failure x -> Printf.printf "%s\n" x; None
  | Parsing.Parse_error -> None
(*  | x -> None*)


(* -------------------------------------------------------------------- *)
(* preprocessing of the patch lines *)

let rec numerate lines =
  let rec outer n = function
      [] -> []
    | (version,lines)::rest ->
	let (n,lines) = inner n lines in
	let rest = outer n rest in
	(version,lines) :: rest
  and inner n = function
      [] -> (n,[])
    | e::rest ->
	let cur = (n,e) in
	let (n,rest) = inner (n+1) rest in
	(n,cur::rest) in
  outer 1 lines

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
      match String.get ln 0 with
	'+' | '-' | ' ' | '@' | 'd' -> true
      | '\\' -> false
      | c ->
	  failwith
	    (Printf.sprintf "%d: unexpected patch character %c\n"
	       patch_line c))
    lines

(* -------------------------------------------------------------------- *)
(* collecting a changed region *)

let drop_initial_spaces all_start_with_star s =
  (* returns string tail + indication of * at start*)
  let len = String.length s in
  let rec loop n =
    if n = len
    then ("",false)
   else
      let char = String.sub s n 1 in
      match char with
	" " | "\n" | "\t" -> loop (n+1)
      |	"*" when all_start_with_star && n <= 1 && not (Aux.substring "*/" s) ->
	  ("",true)
      |	"*" when ((Aux.substring " for " s) || (Aux.substring " while " s) ||
	(Aux.substring " if " s)) -> ("",true)
      |	"*" when n < star_near_the_beginning ->
	  (String.sub s n (String.length s - n),true)
      |	_ -> (String.sub s n (String.length s - n),false) in
  loop 0


let collect_lines bounded str lines =
  let drop_control ln = (* drop +/- *)
    String.sub ln 1 ((String.length ln) - 1) in
  let rec collect_conforming = function
      [] -> ([],0,[])
    | ((n,ln)::rest) as lines ->
	if start_string str ln
	then
	  let (rest,len,after) = collect_conforming rest in
	  let ln = drop_control ln in
	  ((n,ln)::rest,String.length ln + len,after)
	else ([],0,lines) in
  let (region,len,after) = collect_conforming lines in
  if bounded && len > Config.length_threshold
  then ("",after)
  else
    let rec collect_non_comments collected
	seen_comment_starter all_start_with_star =
      function
	[] -> (collected,all_start_with_star)
      |	(n,ln)::rest ->
	  let (ln,starts_with_star) =
	    drop_initial_spaces all_start_with_star ln in
	  let all_start_with_star = starts_with_star && all_start_with_star in
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
	    | None -> (collected^ln^"\n",all_start_with_star) in
	  collect_non_comments collected_so_far seen_comment_starter
	    all_start_with_star rest in
    match collect_non_comments "" false true lines with
      (_,true) when List.length lines > star_threshold (*just comments*) ->
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
  | None -> ("__region__" ^ string_of_int !_compteur)
  (* try get name of func, not just split in region,  otherwise will be split
     wheras in fact in same function *)
  | Some s -> "__infunction__" ^ s (* TODO ? get also compteur,  so sure that
   dont mix together same functions  defined in different files *)

(* processes a single file *)
let process_lines version dirname filename lines =
  let block_end start_line before minus plus after =
    if not (all_blank minus && all_blank plus)
    then
      begin
	let m = before^minus^after in
	let p = before^plus^after in

	Parse_error.start_line := start_line;

      (* ---------------- THIS IS THE IMPORTANT PLACE --------------- *)
	match (parse m, parse p) with
	  (Some mres,Some pres) ->
	    (try
	      let changelists = Diff.diff mres pres in
	      List.map
		(function changelist ->
		  (changelist,(version, dirname, filename, current_region())))
		changelists
	    with Failure s ->
	      (Printf.fprintf stderr
		"%d: failed on %s:\n---\n%s\n+++\n%s\n\n"
		start_line s m p;
	       []))
	| _ -> []
      end
    else [] in
  let rec loop start_count start_line before = function
      (* start count is the number of the region
	 start line is the number of the first line in this region *)
      [] -> ([],[])
    | ((n,ln)::rest) as lines ->
	if start_string "diff " ln or start_string "--- " ln
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
	      if !Config.notex
	      then (Printf.printf "%s region %d:\n\n" filename start_count;
		    flush stdout);
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
	    (String.lowercase (Filename.chop_extension fl))(*drop .c*);
	let (res,after) =
	  process_lines version path !Config.complete_filename rest in
	res@(loop after)
  in loop lines

let process_all_files (lines : (int * string list) list) =
  let lines = numerate lines in
  List.concat (List.map process_file lines)
