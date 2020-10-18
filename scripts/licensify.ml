let lines =
  ["Copyright 2012, INRIA, Julia Lawall";
   "";
   "Patchparse is free software: you can redistribute it and/or modify";
   "it under the terms of the GNU General Public License as published by";
   "the Free Software Foundation, according to version 2 of the License.";
   "";
   "Patchparse is distributed in the hope that it will be useful,";
   "but WITHOUT ANY WARRANTY; without even the implied warranty of";
   "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the";
   "GNU General Public License for more details.";
   "";
   "You should have received a copy of the GNU General Public License";
   "along with Patchparse.  If not, see <http://www.gnu.org/licenses/>.";
   "";
   "The authors reserve the right to distribute this or future versions of";
   "Patchparse under other licenses."
  ]


let comment_lines =
  List.map (function x -> if x <> "" then " * "^x else " *") lines

let cpp_lines = "/*" :: comment_lines @ [" */"]

let ml_lines = "(*" :: comment_lines @ [" *)"]

let make_lines = (List.map (function x -> if x <> "" then "# "^x else "#") lines)
let c_lines = (List.map (function x -> if x <> "" then "// "^x else "//") lines)

let do_one file =
  let lines =
    if Filename.check_suffix file ".cocci" then c_lines else
    if Filename.check_suffix file ".mly"   then cpp_lines else
    if Filename.check_suffix file ".ml"    then ml_lines else
    if Filename.check_suffix file ".mli"   then ml_lines else
    if Filename.check_suffix file ".mll"   then ml_lines else
    if Filename.check_suffix file ".pl"    then make_lines else
    if Filename.basename file = "Makefile" then make_lines else
      failwith (Printf.sprintf "unknown file type: %s" file) in
  let _ = Sys.command (Printf.sprintf "cp %s /tmp/tmpfl" file) in
  let o = open_out file in
  List.iter (function l -> Printf.fprintf o "%s\n" l) lines;
  Printf.fprintf o "\n";
  Printf.fprintf o "\n";
  close_out o;
  let _ = Sys.command (Printf.sprintf "cat /tmp/tmpfl >> %s" file) in
  ()

(* pad's modif *)
let (+>) o f = f o
let cat file =
  let chan = open_in file in
  let rec cat_aux acc ()  =
    (* cant do input_line chan::aux() cos ocaml eval from right to left ! *)
    let (b, l) = try (true, input_line chan) with End_of_file -> (false, "") in
    if b
    then cat_aux (l::acc) ()
    else acc
  in
  cat_aux [] () +> List.rev +> (fun x -> close_in chan; x)


let rec process dir =
  let files =
    try
      List.map (function fl -> dir^"/"^fl)
        (Array.to_list(Sys.readdir dir))
    with Sys_error _ -> [] in
  List.iter (function file ->
    try
      let xs = cat file in
      if List.exists (fun s ->
          s = "* This file is part of Patchparse."
          ||
          s = "# This file is part of Patchparse."
          ||
          s = "// This file is part of Patchparse."
          ||
          Str.string_match (Str.regexp_string "Copyright") s 0
        ) xs
      then print_string ("already processed: " ^ file ^ "\n")
      else begin
        do_one file;
        print_string ("processed: " ^ file ^ "\n");
      end
    with _ ->
      print_string ("skipped: " ^ file ^ "\n");
      ()
    ) files;
  (* pad: no recursive call in directory List.iter process files *)
  ()

let _ = process "."
