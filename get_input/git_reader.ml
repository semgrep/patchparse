let lines = ref 0

let my_input_line i =
  lines := !lines + 1;
  (!lines,input_line i)

let rec drop_git_start i (n,l) =
  let rec loop ver code =
    let (n,l) = my_input_line i in
    match Str.split (Str.regexp " ") l with
      "commit"::_ -> drop_git_start i (n,l)
    | "diff"::_ -> (ver,code,(n,l))
    | _ -> loop ver code in
  match Str.split (Str.regexp " ") l with
    ["commit";code] ->
      let (n,l) = my_input_line i in
      (match
	Str.split (Str.regexp " ") 
	  (List.hd (Str.split (Str.regexp "<") l)) with
	"Author:"::name ->
	  let name = String.concat " " name in
	  let (n,l) = my_input_line i in
	  (match Str.split (Str.regexp " ") l with
	    "Date:"::_::_::_::month::day::_::year::_ ->
	      let date = String.concat " " [month;day;year] in
	      let ver = Printf.sprintf "%s %s %s" code name date in
	      (try loop ver code with End_of_file -> (ver,code,(!lines+1,"")))
	  | _ -> failwith (Printf.sprintf "bad git file %s" l))
      |	"Merge:"::_ ->
	  (try loop "" "" with End_of_file -> ("","",(!lines+1,"")))
      |	_ -> failwith (Printf.sprintf "bad git file %s" l))
  | _ -> failwith "bad git file"

let drop_patch_start i l =
  ("no_ver","no_code",l)

let get_diffs i l =
  let lines = ref [] in
  let rec loop (n,l) =
    match Str.split (Str.regexp " ") l with
      ["commit";code] ->
	let front =
	  match !lines with
	    (_,"") :: rest -> List.rev rest
	  | front -> List.rev front in
	(Some (n,l),front)
    | _ -> lines := (n,l)::!lines; loop (my_input_line i) in
  try loop l with End_of_file -> (None,List.rev !lines)
    
let git fl =
  lines := 0;
  let i = open_in fl in
  let all_lines = ref [] in
  let rec loop l ctr =
    let (info,code,l) = drop_git_start i l in
    let (l,lines) = get_diffs i l in
    all_lines := (ctr,lines) :: !all_lines;
    Hashtbl.add Config.version_table ctr (Printf.sprintf "%s" info);
    match l with None -> () | Some l -> loop l (ctr + 1) in
  loop (my_input_line i) 0;
  close_in i;
  List.rev !all_lines

let patch fl =
  lines := 0;
  let i = open_in fl in
  let all_lines = ref [] in
  let rec loop l ctr =
    let (info,code,l) = drop_patch_start i l in
    let (l,lines) = get_diffs i l in
    all_lines := (ctr,lines) :: !all_lines;
    Hashtbl.add Config.version_table ctr (Printf.sprintf "%d" ctr);
    match l with None -> () | Some l -> loop l (ctr + 1) in
  loop (my_input_line i) 0;
  close_in i;
  List.rev !all_lines
