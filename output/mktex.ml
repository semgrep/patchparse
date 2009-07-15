module CE = Ce
let url = "http://grmso.net:8090/commit/"
let url = "http://git.kernel.org/?p=linux/kernel/git/torvalds/linux-2.6.git;a=commit;h="
(* -------------------------------------------------------------------- *)
(* Print the data in the VERSION or DIRECTORY cases *)

let split_git_version version =
  match Str.split (Str.regexp " ") version with
    git_code :: rest -> let rest = String.concat " " rest in (git_code,rest)
  | _ -> failwith "bad version"

(* -------------------------------------------------------------------- *)
(* Latex: printing the results *)

let tex_prolog o =
  Printf.fprintf o "\\IfFileExists{preamble.tex}{\\input{preamble.tex}}{%%\n";
  Printf.fprintf o "\\documentclass[a4paper,article,oneside]{memoir}}\n";
  Printf.fprintf o
    "\\IfFileExists{usersetup.sty}{\\usepackage{usersetup}}{%%\n";
  Printf.fprintf o "\\usepackage{listings}\n";
  Printf.fprintf o "\\usepackage{hyperref}\n";
  Printf.fprintf o "\\usepackage{fullpage}\n";
  Printf.fprintf o "\\usepackage{memhfixc}\n";
  Printf.fprintf o "\\renewcommand{\\ttdefault}{pcr}\n";
  Printf.fprintf o "\\setsecnumdepth{part}\n";
  Printf.fprintf o "\\lstset{language=C,columns=fullflexible,basicstyle=\\ttfamily\\small,breaklines=true,xleftmargin=1cm,xrightmargin=1cm}\n";
  Printf.fprintf o "\\hypersetup{colorlinks=true}\n";
  Printf.fprintf o "\\pagestyle{plain}\n";
  Printf.fprintf o "\\advance\\hoffset by -1.75cm\n";
  Printf.fprintf o "\\textwidth 7.5in}\n";

  Printf.fprintf o "\\begin{document}\n"

let tex_epilog o =
  Printf.fprintf o "\\end{document}\n"

let file_data tex_file out_file (printer : 'change -> string)
    (printer_parsable : 'change -> string)
    ((version_table,dir_table,multidir_table,multiver_table1,multiver_table2) :
       Questions.result) =
  let printed_changes = ref false in
  let pc data = (* version or directory *)
    if not(!printed_changes)
    then
      begin
	List.iter
	  (function (tag,data) ->
	    Printf.fprintf tex_file "\\noindent\\textbf{%s}\n\n"
	      (CE.clean tag);
	    List.iter
	      (function (change,count) ->
		(if !Config.print_parsable
		then Printf.fprintf out_file "%s\n" (printer_parsable change));
		Printf.fprintf tex_file
		  "\\noindent\\,\\,\\,\\, %d changes: %s\n\n"
		  count (printer change))
	      (List.rev data))
	  data
      end in
  let pcm data = (* multidirectory *)
    Printf.fprintf tex_file "%d changes in all\n\n" (List.length data);
      let ct = ref 0 in
      List.iter
        (function (change,data) ->
          ct := !ct + 1;
          Printf.fprintf tex_file
            "\\vspace*{1.5em}\\noindent %d. %d(%d). %s\n\n%s\n\n"
            !ct
            (let (directories,counts) = List.split data in
            Aux.sum counts)
            (List.length data) (printer change)
            (if !Config.git
            then
              (String.concat ""
                 (let seen = Hashtbl.create(List.length data) in
                 let rec loop prev = function
                     [] -> []
                   | ((version,dir),count)::rest ->
                       let version = CE.clean version in
                       let dir = CE.clean dir in
                       let (prev,front) =
                         let (git_code,rest) = split_git_version version in
                         (git_code,
                          if prev = git_code
                          then
                            Printf.sprintf "{\\mbox{%s (%d)}} " dir count
                          else
                            Printf.sprintf
                              "\n\n\\lefteqn{\\mbox{\\href{%s%s}{%s}: \\emph{%s\
} %s (%d)}}\n\n"
                              url git_code git_code rest dir count) in
                       front :: (loop prev rest) in
                 loop "" data))
            else
              (let per_line = 2 in
              String.concat ", "
                (let rec loop n = function
                    [] -> []
                  | ((version,dir),count)::rest ->
                      let version = CE.clean version in
                      let dir = CE.clean dir in
                      let front =
                        Printf.sprintf "%s: %s (%d)"
                          version dir count in
                      if n = per_line
                      then (front ^ "\n\n") :: (loop 0 rest)
                      else front :: (loop (n+1) rest) in
                loop 0 data))))
            data in
  let process info_string table pc =
    Printf.fprintf tex_file "\\section{By %s}\n" info_string;
    List.iter
      (function (version,data) ->
	let version = CE.clean version in
	if info_string = "version"
	then
	  begin
	    let unused_tokens =
	      try !(Hashtbl.find Eqclasses.version_unused_table version)
	      with Not_found -> 0 in
	    let (git_code,rest) = split_git_version version in
	    Printf.fprintf tex_file
	      "\\subsection{\\href{%s%s}{%s}} \\emph{%s}\n\n\\noindent %d dirs %d unused tokens\n\n\\bigskip\n\n" 
	      url git_code git_code rest (List.length data) unused_tokens
	  end
	else
	  Printf.fprintf tex_file "\\subsection{%s: %d}\n" version
	    (List.length data);
	pc data)
   table in
  if not (version_table = [])
  then
    (process "version" version_table pc;
     printed_changes := true);
  if not (multidir_table = [])
  then
    process
      (if !Config.git then "change" else "multi-directory")
      [("change",multidir_table)] pcm

let print_evolutions tex_file evolutions =
  if List.exists (function (_,_,_,x,_) -> List.length x > 1) evolutions
  then
    (Printf.fprintf tex_file "\\section{Evolutions}\n";
     List.iter
       (function (version,dir,intersection,changes,weight) ->
	 let v = Config.get_version version in
	 Printf.fprintf tex_file "\\subsection{%s. %s %f}files:\n\n"
	   v dir weight;
	 List.iter
	   (function file ->
	     Printf.fprintf tex_file "  \\noindent\\hspace{-0.25in}%s\n\n"
	       (CE.clean file))
	   intersection;
	 Printf.fprintf tex_file "\n\\noindent changes:\n\n";
	 List.iter
           (function ce ->
	     Printf.fprintf tex_file "  \\noindent\\hspace{-0.25in}%s\n\n"
	       (CE.ce2tex ce))
	   changes)
       evolutions)
      
(* -------------------------------------------------------------------- *)
      
let safe_div str num = function
    0 -> (*Printf.printf "zero denominator for %s" str;*) 0
  | den -> num / den

let print_summary tex_file desired_info
    (label, (change_table: Eqclasses.change_table_type),
     (version_table,dir_table,multidir_table,
      multiver_table1,multiver_table2)) =
  let change_table_changes = Hashtbl.length change_table in
  let change_table_sites =
    Hashtbl.fold
      (function change ->
	function data ->
	  function sum ->
	    sum + Aux.sum (List.map (function (_,sites,_,_,_) -> sites) data))
      change_table 0 in
  let change_table_files =
    Hashtbl.fold
      (function change ->
	function data ->
	  function sum ->
	    sum +
	      Aux.sum
	      (List.map (function (_,_,files,_,_) -> List.length files) data))
      change_table 0 in
  let max_change_table_files =
    Hashtbl.fold
      (function change ->
	function data ->
	  function ans ->
	    let tmp =
	      Aux.sum
		(List.map (function (_,_,files,_,_) -> List.length files)
		   data) in
	    if tmp > ans then tmp else ans)
      change_table 0 in
  let multi_total_changes = List.length multidir_table in
  let multi_change_sites =
    Aux.sum
      (List.map
	 (function (change,sites) ->
	   Aux.sum (List.map (function (_,ct) -> ct) sites))
	 multidir_table) in
  let max_change_sites =
    List.fold_left
      (function ans ->
	function (change,sites) ->
	  let site_count =
	    Aux.sum (List.map (function (_,ct) -> ct) sites) in
          if site_count > 1000
	  then Printf.fprintf stderr "BIGGGGGGGGGGGG: %s" (CE.ce2tex change);
	  if site_count > ans then site_count else ans)
      0 multidir_table in
  Printf.fprintf tex_file "\\subsection{%s}\n\n" label;
  Printf.fprintf tex_file "Total changes %d %d\n\n"
    change_table_changes multi_total_changes;
  Printf.fprintf tex_file "Total change sites %d %d\n\n"
    change_table_sites multi_change_sites;
  Printf.fprintf tex_file "Average sites per change %d\n\n"
    (safe_div "Average sites per change %d\n\n"
       change_table_sites change_table_changes);
  Printf.fprintf tex_file "Max sites per change %d\n\n" max_change_sites;
  Printf.fprintf tex_file "Total files*changes %d\n\n" change_table_files;
  Printf.fprintf tex_file "Average files per change %d\n\n"
    (safe_div "Average files per site %d\n\n"
       change_table_files change_table_changes);
  Printf.fprintf tex_file "Max files per change %d\n\n"
    max_change_table_files;
  Printf.fprintf tex_file "Affected versions %d\n\n"
    (List.length version_table);
  Printf.fprintf tex_file "Affected directories %d\n\n"
    (List.length dir_table);
  Printf.fprintf tex_file "\\begin{tabular}{lccc}\n";
  Printf.fprintf tex_file "&changes&sites&site/change\\\\\n";
  List.iter
    (function (dir,data) ->
      let changes =
	let change_table =
	  (Hashtbl.create(200) : (CE.ce,unit) Hashtbl.t) in
	List.iter
	  (function (version,changes) ->
	    List.iter
	      (function (change,ct) ->
		try
		  let _ = Hashtbl.find change_table change in ()
		with
		  Not_found -> Hashtbl.add change_table change ())
	      changes)
	  data;
	Hashtbl.length change_table in
      let sites =
	Aux.sum
	  (List.map
	     (function (version,changes) ->
	       Aux.sum (List.map (function (change,ct) -> ct) changes))
	     data) in
      Printf.fprintf tex_file "%s & %d & %d & %d\\\\\n"
	dir changes sites (safe_div "sites per change" sites changes))
    dir_table;
  Printf.fprintf tex_file "\\end{tabular}";
  (change_table_changes,change_table_sites,max_change_sites,
   change_table_files,max_change_table_files)

(* -------------------------------------------------------------------- *)
(* Entry point *)
	
let make_files (change_result,filtered_results) evolutions =
  let all_name = "_"^(!Config.file) in
  let tex_file =
    open_out (Printf.sprintf "%s/all%s.tex" !Config.out_dir all_name) in
  let out_file =
    if !Config.print_parsable
    then open_out (Printf.sprintf "%s/all%s.out" !Config.out_dir all_name)
    else open_out "/dev/null" in
  tex_prolog tex_file;
  if not !Config.noall
  then
    begin
      Printf.fprintf tex_file "\\chapter{All changes}\n";
      file_data tex_file out_file CE.ce2tex CE.ce2parsable change_result;
      print_evolutions tex_file evolutions;
    end;
  List.iter
    (function (label,change_table,change_result) ->
      Printf.fprintf tex_file "\\chapter{%s}\n" label;
      file_data tex_file out_file
	CE.ce2tex (function _ -> "") change_result)
    filtered_results;
  close_out out_file;
  tex_epilog tex_file;
  close_out tex_file;
  if not (!Config.notex)
  then
    let cmdfunc = "pdflatex" in
    let _ =
      Sys.command
	(Printf.sprintf "%s %s/all%s.tex" cmdfunc !Config.out_dir all_name) in
    let _ =
      Sys.command
	(Printf.sprintf "%s %s/all%s.tex" cmdfunc !Config.out_dir all_name) in
    ()
