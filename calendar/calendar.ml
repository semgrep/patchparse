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

let check = function
    0 -> ()
  | x -> failwith (Printf.sprintf "returned %d" x)

let months =
  [("Jan",(1,31));
   ("Feb",(2,28));
   ("Mar",(3,31));
   ("Apr",(4,30));
   ("May",(5,31));
   ("Jun",(6,30));
   ("Jul",(7,31));
   ("Aug",(8,31));
   ("Sep",(9,30));
   ("Oct",(10,31));
   ("Nov",(11,30));
   ("Dec",(12,31))]

let get_commit dir =
  let cmd =
    Printf.sprintf "cd %s; git log | grep ^commit | head -1" dir in
  match cmd_to_list cmd with
    [commit] ->
    (match Str.split (Str.regexp " ") commit with
       [_;commit] -> commit
     |	_ -> failwith "bad commit")
  | _ -> failwith "bad commit"

let commit_from_file file =
  let cmd =
    Printf.sprintf "grep ^commit %s | head -1" file in
  match cmd_to_list cmd with
    [commit] ->
    (match Str.split (Str.regexp " ") commit with
       [_;commit] -> Some commit
     |	_ -> failwith "bad commit")
  | [] -> None
  | _ -> failwith "bad commit"

let get_date dir =
  let cmd =
    Printf.sprintf
      "cd %s; git log  --pretty=fuller | grep ^CommitDate: | head -1"
      dir in
  match cmd_to_list cmd with
    [date] ->
    (match Str.split (Str.regexp "[ \t]+") date with
       _::_::month::day::time::year::_ ->
       let day =
         try int_of_string day
         with Failure _ -> failwith ("bad day: "^day) in
       let year =
         try int_of_string year
         with Failure _ -> failwith ("bad day: "^year) in
       (month,day,year)
     |	_ -> failwith "bad date")
  | _ -> failwith "bad date"

let next_day (month,day,year) inc =
  (if inc > 28 then failwith "increment cannot be greater than 28");
  let rec matchfrom = function
      [] -> failwith ("bad month: "^month)
    | ((x,_) as h)::xs ->
      if x = month
      then (h,xs)
      else matchfrom xs in
  let ((mo,(_,lastday)),rest) = matchfrom months in
  if day + inc > lastday
  then
    match rest with
      [] ->
      (match months with
         (mo,_)::_ -> (mo,day + inc - lastday,year+1)
       |	_ -> failwith "impossible")
    | (mo,_)::_ -> (mo,day + inc - lastday,year)
  else (month,day+inc,year)

(* ------------------------------------------------------------------ *)

type date = Date of (string * int * int) | Commit of string

let geq (mo,day,yr) (nmo,nday,nyr) =
  (yr > nyr) ||
  (yr = nyr &&
   let m = fst (List.assoc mo months) in
   let nm = fst (List.assoc nmo months) in
   ((m > nm) ||
    (m = nm && day >= nday)))

let get_files date start finish repo out increment restrict =
  let rec loop acc start_date start =
    let continue (mo,day,yr) ((nmo,nday,nyr) as next) =
      let pre_dest =
        Printf.sprintf "%s/%s_%d_%d_%s_%d_%d" out mo day yr nmo nday nyr in
      let dest = Printf.sprintf "%s/%s" (Sys.getcwd()) pre_dest in
      let cmd = Printf.sprintf "/bin/rm -rf %s; mkdir -p %s\n" dest dest in
      check (Sys.command cmd);
      let until =
        (*if next = finish
               then ""
          	else*) Printf.sprintf "--until=\"%s %d %d\" " nmo nday nyr in
      let dest = dest^"/gitlog" in
      let cmd =
        Printf.sprintf
          "cd %s; git log --no-merges -p --since=\"%s %d %d\" %s %s > %s"
          repo mo day yr until restrict dest in
      Printf.printf "%s\n" cmd; flush stdout;
      check (Sys.command cmd);
      if geq next finish
      then List.rev (pre_dest::acc)
      else
        (match commit_from_file dest with
           None ->
           loop (pre_dest::acc) next
             (Commit "no idea what the commit is used for")
         | Some last_commit ->
           loop (pre_dest::acc) next (Commit last_commit)) in
    match start with
      Date date ->
      let next =
        if increment = -1
        then finish
        else next_day date increment in
      continue date next
    | Commit commit ->
      let next =
        if increment = -1
        then finish
        else next_day start_date increment in
      continue start_date next in
  loop [] date start

let get_files_by_version date start finish repo out restrict =
  let pre_dest = Printf.sprintf "%s/%s_%s" out start finish in
  let dest = Printf.sprintf "%s/%s" (Sys.getcwd()) pre_dest in
  let cmd = Printf.sprintf "/bin/rm -rf %s; mkdir -p %s\n" dest dest in
  check (Sys.command cmd);
  let dest = dest^"/gitlog" in
  let cmd =
    Printf.sprintf "cd %s; git log --no-merges -p %s..%s %s> %s"
      repo start finish restrict dest in
  Printf.printf "%s\n" cmd; flush stdout;
  check (Sys.command cmd);
  [pre_dest]

let run_patchparse patchparse repo linuxnext extraopts url spinfer file =
  let url =
    if not (url = "")
    then url
    else
    if linuxnext
    then "https://git.kernel.org/cgit/linux/kernel/git/next/linux-next.git/commit/?id="
    else "https://git.kernel.org/cgit/linux/kernel/git/torvalds/linux.git/commit/?id=" in
  let options = "--min 10 --noev --noall --print-sp "^extraopts in
  let spinfer = if spinfer = "" then "" else ("--spinfer "^spinfer) in
  let cmd =
    Printf.sprintf
      "%s --git %s/gitlog %s --out-dir %s --gitdir %s --giturl %s %s"
      patchparse file options file repo url spinfer in
  Printf.printf "%s\n" cmd; flush stdout;
  check (Sys.command cmd)

let create_make out files =
  let o = open_out (Printf.sprintf "%s/Makefile" out) in
  Printf.fprintf o "all: spinferall\n";
  let files = List.map Filename.basename files in
  let names =
    List.mapi
      (fun i x ->
         let name = Printf.sprintf "spi%d" i in
         Printf.fprintf o "%s:\n" name;
         Printf.fprintf o "\tcd %s; make -f allgitlog.make -k spinferall\n" x;
         name)
      (List.rev files) in
  Printf.fprintf o "spinferall: %s\n" (String.concat " " names);
  close_out o

(* ------------------------------------------------------------------ *)

let start_date = ref ""
let end_date = ref ""
let increment = ref (-1)
let repository = ref "/home/julia/linux-next"
let out_dir = ref "output"
let update_command = ref "./update_linux-next"
let patchparse = ref "/home/julia/patchparse3/patchparse.opt"
let patchparse = ref "/home/julia/patchparse3/patchparse"
let restrict = ref ""
let args = ref ""
let url = ref ""
let gitsearch_args = ref ""
let prequel_file = ref ""
let spinfer_path = ref ""

type mode = Patchparse | Gitsrch | Prequel
let mode = ref Patchparse

let reset = ref true

let speclist = Arg.align
    ["--start", Arg.Set_string start_date, "  starting date";
     "--end", Arg.Set_string end_date, "  ending date";
     "--out", Arg.Set_string out_dir, "  output directory";
     "--inc", Arg.Set_int increment, "  date increment";
     "--update", Arg.Set_string update_command, "  update command";
     "--restrict", Arg.Set_string restrict, "  subdir to consider";
     "--args", Arg.Set_string args, "  other args for patchparse, must be quoted";
     "--url", Arg.Set_string url, "  url for links";
     "--gitsearch", Arg.String (fun s -> gitsearch_args := s; mode := Gitsrch),
     "  args for git search, must be quoted";
     "--prequel", Arg.String (fun s -> prequel_file := s; mode := Prequel),
     "  file with prequel output";
     "--spinfer", Arg.Set_string spinfer_path, "  location of spinfer";
     "--noreset", Arg.Clear reset, "  don't reset git, allows concurrency"]

let usage = "Usage: cocci_infer repository [--start date] [--end date], etc"

let anonymous str = repository := str

let parse_date s =
  let (s,ver) =
    if not (String.length s = 6) || String.get s 0 = 'v'
    then
      let cmd =
        Printf.sprintf
          "cd %s; git log -n1 --pretty=format:%%cd --date=format:%%m%%d%%y %s"
          !repository s in
      (List.hd (cmd_to_list cmd),Some s)
    else (s,None) in
  let month = int_of_string (String.sub s 0 2) in
  let day = int_of_string (String.sub s 2 2) in
  let year = int_of_string ("20" ^ (String.sub s 4 2)) in
  let mo =
    try fst (List.nth months (month - 1))
    with Failure _ -> failwith "bad month" in
  ((mo,day,year),ver)

let _ =
  Arg.parse speclist anonymous usage;
  (if !reset
   then
     check
       (Sys.command
          (Printf.sprintf "cd %s; git reset --hard\n" !repository)));
  let commit = get_commit !repository in
  let date = get_date !repository in
  let cmd = Printf.sprintf "cd %s/..; %s" !repository !update_command in
  Printf.printf "%s\n" cmd;
  check (Sys.command cmd);
  let new_date = get_date !repository in
  match !mode with
    Patchparse ->
    let start =
      if !start_date = ""
      then (Commit commit,None)
      else
        let dt = parse_date !start_date in
        (Date(fst dt),snd dt) in
    let finish =
      if !end_date = ""
      then (new_date,None)
      else parse_date !end_date in
    let files =
      match (start,finish) with
        ((start,None),(finish,None)) ->
        get_files date start finish !repository !out_dir !increment
          !restrict
      | ((_,Some v1),(_,Some v2)) ->
        get_files_by_version date v1 v2 !repository !out_dir !restrict
      | ((_,Some v1),(_,None)) ->
        get_files_by_version date v1 "" !repository !out_dir !restrict
      | ((_,None),(_,Some v2)) ->
        get_files_by_version date "" v2 !repository !out_dir !restrict in
    let linuxnext =
      try
        let _ = Str.search_forward (Str.regexp "linux-next") !repository 0 in
        true
      with Not_found -> false in
    create_make !out_dir files;
    List.iter
      (run_patchparse !patchparse !repository linuxnext !args !spinfer_path
         !url)
      files
  | Gitsrch ->
    Gitsearch.gitsearch !repository !start_date !end_date !gitsearch_args
      !out_dir !spinfer_path
  | Prequel -> Gitsearch.prequel !repository !prequel_file !spinfer_path
