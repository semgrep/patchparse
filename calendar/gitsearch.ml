let process_output_to_list2 = fun command ->
  let chan = Unix.open_process_in command in
  let res = ref ([] : string list) in
  let rec process_otl_aux () =
    let e = input_line chan in
    res := e::!res;
    process_otl_aux() in
  try process_otl_aux ()
  with End_of_file ->
    let stat = Unix.close_process_in chan in (List.rev !res,stat)
let cmd_to_list command =
  let (l,_) = process_output_to_list2 command in l
let process_output_to_list = cmd_to_list
let cmd_to_list_and_status = process_output_to_list2

let process_all_commits repository out commits =
  let o = open_out (out^"/index") in
  let home = Sys.getcwd() in
  let _ =
    Sys.command
      (Printf.sprintf "/bin/rm -f %s/gitlog; touch %s/gitlog" out out) in
  List.iter
    (function commit ->
       let n =
         Sys.command
           (Printf.sprintf "cd %s; git show %s >> %s/%s/gitlog" repository
              commit home out) in
       if not (n = 128) (* 128 = failure *)
       then
         begin
           let cmd =
             Printf.sprintf "cd %s; git show --name-only --pretty=format:--- %s"
               repository commit in
           let files =
             match cmd_to_list cmd with
               "---"::rest -> rest
             | _::"---"::rest -> rest
             | _ -> failwith "bad files" in
           let files =
             List.filter
               (fun x ->
                  Filename.check_suffix x ".c" || Filename.check_suffix x ".h")
               files in
           let files =
             List.map
               (function fl ->
                  let newname =
                    String.concat "__" (Str.split (Str.regexp "/") fl) in
                  let pieces = Str.split (Str.regexp_string ".") newname in
                  (fl,
                   Printf.sprintf "%s_%s" (List.nth pieces 0) commit,
                   List.nth pieces 1))
               files in
           List.iter
             (function (fl,refl,ext) ->
                ignore
                  (Sys.command
                     (Printf.sprintf "cd %s; git show %s^:%s > %s/%s/%s.%s"
                        repository commit fl home out refl ext));
                ignore
                  (Sys.command
                     (Printf.sprintf "cd %s; git show %s:%s > %s/%s/%s.res.%s"
                        repository commit fl home out refl ext));
                Printf.fprintf o "%s.%s %s.res.%s\n" refl ext refl ext)
             files
         end)
    commits;
  close_out o

(* entry points *)

let gitsearch repository start finish args out spinfer =
  let spinfer = if spinfer = "" then "spinfer" else spinfer in
  let cmd =
    Printf.sprintf "cd %s; git log %s %s %s --pretty=format:%%h" repository
      (if start = "" then "" else Printf.sprintf "--since=\"%s\"" start)
      (if finish = "" then "" else Printf.sprintf "--until=\"%s\"" finish)
      args in
  let commits = cmd_to_list cmd in
  let _ = Sys.command (Printf.sprintf "/bin/rm -rf %s; mkdir %s" out out) in
  process_all_commits repository out commits;
  let o = open_out(out^"/Makefile") in
  Printf.fprintf o "SPICMD=%s -f index -o spinfer.cocci -d spinfer.cocci.debug > spinfer.out 2> spinfer.tmp\n\n" spinfer;
  Printf.fprintf o "all: index\n\t$(SPICMD)\n";
  close_out o

let prequel repository file spinfer =
  let spinfer = if spinfer = "" then "spinfer" else spinfer in
  let out = Filename.basename file in
  let _ = Sys.command (Printf.sprintf "/bin/rm -rf %s; mkdir %s" out out) in
  let porgs =
    cmd_to_list (Printf.sprintf "find . -name \"%s/step*porg\"" file) in
  let seen = ref [] in
  let reporgs =
    List.map
      (function porg ->
         let ender = Filename.chop_extension (Filename.basename porg) in
         let ender =
           if List.mem ender !seen
           then Printf.sprintf "%s_%d" ender (List.length !seen)
           else ender in
         seen := ender :: !seen;
         (porg, ender))
      porgs in
  List.iter
    (function (porg,reporg) ->
       let commits =
         cmd_to_list
           (Printf.sprintf "grep ^'*' %s | cut -d: -f1 | cut -d' ' -f2" porg) in
       let local = Printf.sprintf "%s/%s" out reporg in
       let _ = Sys.command ("mkdir "^local) in
       process_all_commits repository local commits)
    reporgs;
  let o = open_out (out^"/Makefile") in
  Printf.fprintf o "HERE=`pwd`\n";
  Printf.fprintf o "SPICMD=cd $(HERE)/$<; %s -f index -o spinfer.cocci -d spinfer.cocci.debug > spinfer.out 2> spinfer.tmp\n\n" spinfer;
  Printf.fprintf o "all: %s\n\n"
    (String.concat " " (List.map (fun (_,x) -> x^".spi") reporgs));
  List.iter
    (function (porg,reporg) ->
       Printf.fprintf o "%s.spi: %s\n\t$(SPICMD)\n\n" reporg reporg)
    reporgs;
  close_out o
