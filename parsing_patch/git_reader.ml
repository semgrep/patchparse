module Config = Globals

open Patch_reader

let rec drop_git_start in_lines =
  let rec loop ver code = function
      [] -> (ver,code,[])
    | (n,l)::rest ->
      match Str.split_delim (Str.regexp " ") l with
        "commit"::_ -> drop_git_start ((n,l)::rest)
      | "diff"::_ -> (ver,code,((n,l)::rest))
      | "---"::_ -> (ver,code,((n,l)::rest))
      | _ -> loop ver code rest in
  match in_lines with
    [] -> failwith "should not happen"
  | (nc,lc)::(na,la)::(nd,ld)::rest ->
    (match Str.split_delim (Str.regexp " ") lc with
       ["commit";code] ->
       (match
          Str.split (Str.regexp " ")
            (List.hd (Str.split (Str.regexp "<") la)) with
         "Author:"::name ->
         let name = String.concat " " name in
         (match Str.split (Str.regexp " ") ld with
            "Date:"::_::_::_::month::day::_::year::_ ->
            let date = String.concat " " [month;day;year] in
            let ver = Printf.sprintf "%s %d %s %s" code nc name date in
            loop ver code rest
          | _ ->
            failwith
              (Printf.sprintf "%s: date: bad git file %s" code ld))
       | "Merge:"::_ -> loop "" "" rest
       | _ ->
         failwith
           (Printf.sprintf "%s: after commit: bad git file %s" code la))
     | _ -> drop_git_start ((na,la)::(nd,ld)::rest))
  | _ -> failwith "bad git file"


(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let git fl =
  lines := 0;
  let in_lines =
    match () with
    | _ when !Config.gitpatch ->
      Aux.cmd_to_list (Printf.sprintf "/bin/cat %s" fl)
    | _ when !Config.gitcommitlist ->
      let commits = Aux.cmd_to_list (Printf.sprintf "/bin/cat %s" fl) in
      List.fold_left (fun prev commit ->
          (Aux.cmd_to_list
             (Printf.sprintf "cd %s ; /usr/bin/git show %s"
                !Config.gitdir commit)) @ prev
        )
        [] commits
    | _ -> 
      Aux.cmd_to_list
        (Printf.sprintf "cd %s ; /usr/bin/git log -p %s %s"
           !Config.gitdir fl !Config.git_restrict) 
  in
  let in_lines = counter in_lines in
  let rec loop acc in_lines ctr =
    match in_lines with
    | [] -> List.rev acc
    | in_lines ->
      let (info,code,in_lines) = drop_git_start in_lines in
      let (lines,in_lines) = get_diffs in_lines in
      Hashtbl.add Config.version_table ctr (Printf.sprintf "%s" info);
      loop ((Patch.Id ctr,lines) :: acc) in_lines (ctr + 1) in
  loop [] in_lines 0

