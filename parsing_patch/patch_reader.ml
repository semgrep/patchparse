module Config = Globals

(* conventions: 
 *  - in_lines = input_lines 
 *  - code = commitid
 *  - version = readable version (commitid + date + name)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let lines = ref 0

let drop_patch_start in_lines =
  ("no_ver","no_code",in_lines)

let get_diffs in_lines =
  let rec loop acc = function
      [] -> (List.rev acc,[])
    | (n,"")::rest -> loop acc rest
    | (n,l)::rest ->
      match Str.split (Str.regexp " ") l with
        ["commit";code] -> (List.rev acc,(n,l)::rest)
      | _ ->
        loop ((n,l) :: acc) rest in
  loop [] in_lines

let counter l =
  let rec loop acc n = function
    | [] -> List.rev acc
    | x::xs -> loop ((n,x) :: acc) (n+1) xs in
  loop [] 1 l

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let patch fl =
  lines := 0;
  let in_lines = Aux.cmd_to_list (Printf.sprintf "/bin/cat %s" fl) in
  let in_lines = counter in_lines in
  let rec loop in_lines ctr =
    match in_lines with
      [] -> []
    | in_lines ->
      let (info,code,in_lines) = drop_patch_start in_lines in
      let (lines,in_lines) = get_diffs in_lines in
      Hashtbl.add Config.version_table ctr (Printf.sprintf "%s" info);
      (Patch.Id ctr,lines) :: loop in_lines (ctr + 1) in
  loop in_lines 0
