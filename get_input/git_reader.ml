let rec drop_git_start i l =
  let rec loop ver code =
    let l = input_line i in
    match Str.split (Str.regexp " ") l with
      "commit"::_ -> drop_git_start i l
    | "diff"::_ -> (ver,code,l)
    | _ -> loop ver code in
  match Str.split (Str.regexp " ") l with
    ["commit";code] ->
      let l = input_line i in
      (match
	Str.split (Str.regexp " ") 
	  (List.hd (Str.split (Str.regexp "<") l)) with
	"Author:"::name ->
	  let name = String.concat " " name in
	  let l = input_line i in
	  (match Str.split (Str.regexp " ") l with
	    "Date:"::_::_::_::month::day::_::year::_ ->
	      let date = String.concat " " [month;day;year] in
	      let ver = Printf.sprintf "%s %s %s" code name date in
	      (try loop ver code with End_of_file -> (ver,code,""))
	  | _ -> failwith (Printf.sprintf "bad git file %s" l))
      |	"Merge:"::_ ->
	  (try loop "" "" with End_of_file -> ("","",""))
      |	_ -> failwith (Printf.sprintf "bad git file %s" l))
  | _ -> failwith "bad git file"

let drop_patch_start i l =
  ("no_ver","no_code",l)

let get_diffs i l =
  let lines = ref [] in
  let rec loop l =
    match Str.split (Str.regexp " ") l with
      ["commit";code] -> (Some l,List.rev !lines)
    | _ -> lines := l::!lines; loop (input_line i) in
  try loop l with End_of_file -> (None,List.rev !lines)
    
let git fl =
  let i = open_in fl in
  let all_lines = ref [] in
  let rec loop l ctr =
    let (info,code,l) = drop_git_start i l in
    let (l,lines) = get_diffs i l in
    all_lines := (ctr,lines) :: !all_lines;
    Hashtbl.add Config.version_table ctr (Printf.sprintf "%s" info);
    match l with None -> () | Some l -> loop l (ctr + 1) in
  loop (input_line i) 0;
  close_in i;
  List.rev !all_lines

let patch fl =
  let i = open_in fl in
  let all_lines = ref [] in
  let rec loop l ctr =
    let (info,code,l) = drop_patch_start i l in
    let (l,lines) = get_diffs i l in
    all_lines := (ctr,lines) :: !all_lines;
    Hashtbl.add Config.version_table ctr (Printf.sprintf "%d" ctr);
    match l with None -> () | Some l -> loop l (ctr + 1) in
  loop (input_line i) 0;
  close_in i;
  List.rev !all_lines
