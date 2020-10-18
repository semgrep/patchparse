
type atfront = FRONT | AFTER

let start_line = ref 0

(*** input handle ***)
type handle =
    bool * string * string * int * int * out_channel * string

let current_handle = 
  ref (false, "", "", 0, 0, stdout, "")

let interactive (h : handle) = let (i, _, _, _, _, _, _) = h in i
(*
let line (h : handle) = let (_, l, _, _, _, _, _) = h in l
let buffer (h : handle) = let (_, _, b, _, _, _, _) = h in b
let pos (h : handle) = let (_, _, _, p, _, _, _) = h in p
*)
let real_pos (i : int) (h : handle) = let (_, _, _, p, _, _, _) = h in i - p
let lineno (h : handle) = let (_, _, _, _, n, _, _) = h in n
let out_channel (h : handle) = let (_, _, _, _, _, out, _) = h in out
let file_name (h : handle) = let (_, _, _, _, _, _, name) = h in name


(*** syntax error building ***)
let underline_error (buffer : string) (start : int) (stop : int) =
  let len = String.length buffer in
  let start' = max 0 start in
  let stop' = max 1 stop in
  (
  (if start' > 0 then (String.sub buffer 0 start') else "")
  ^ "\027[4m"
  ^ (if (stop' - start') <> 0
  then (String.sub buffer start' (stop' - start' ) )
  else ""
      )
  ^ "\027[0m"
  ^ (if stop' < len then (String.sub buffer stop' (len - stop') ) else "")
      )

let display_error msg token_start token_end =
  let string =
    if (interactive !current_handle)
    then ""
    else 
       (file_name !current_handle) ^ "["
       ^ (string_of_int (lineno !current_handle)) ^ "] " 
  in
  let oc = out_channel !current_handle in
  Printf.fprintf oc "%s%s : lines from %d start %d stop %d\n%s\n" string msg
    !start_line
    (real_pos token_start !current_handle)
    (real_pos token_end !current_handle)
    (underline_error (file_name !current_handle)
       (real_pos token_start !current_handle)
       (real_pos token_end !current_handle)
       )
    ;
(*
  output_string oc (
  string
  ^ msg ^
  ^ (underline_error
       (line !current_handle)
       (real_pos token_start !current_handle)
       (real_pos token_end !current_handle)
       )
      )*)
  flush (out_channel !current_handle)
