let rec union xs ys =
  match xs with
    [] -> ys
  | x::xs -> if List.mem x ys then union xs ys else x :: (union xs ys)

let rec union_list = function
    [] -> []
  | x::xs -> if List.mem x xs then union_list xs else x :: (union_list xs)

let rec intersect xs ys =
  match xs with
    [] -> []
  | x::xs -> if List.mem x ys then x :: (intersect xs ys) else intersect xs ys

let rec sorted_intersection xs ys =
  match (xs,ys) with
    ([],_) | (_,[]) -> []
  | (x::xs,y::ys) ->
    (match compare x y with
       0 -> x :: sorted_intersection xs ys
     |	n when n < 0 -> sorted_intersection xs (y::ys)
     |	n when n > 0 -> sorted_intersection (x::xs) ys
     |	_ -> failwith "not possible" )

let rec sorted_superset superset subset =
  match (superset,subset) with
    (_,[]) -> true
  | ([],_) -> false
  | (sup::superset,sub::subset) ->
    (match compare sup sub with
       0 -> sorted_superset superset subset
     |	n when n < 0 ->
       sorted_superset superset (sub::subset) (*might still find sub later*)
     |	n when n > 0 -> false (* sup>sub, so we will never see sub *)
     |	_ -> failwith "not possible" )

let rec sorted_difference (xs : 'a list) (ys : 'a list) :
  'a list (* in xs, not ys *) *
  'a list (* in both *) *
  'a list (* in ys, not xs *) =
  match (xs,ys) with
    ([],_) -> ([],[],ys)
  | (_,[]) -> (xs,[],[])
  | (x::xs,y::ys) ->
    (match compare x y with
       0 ->
       let (xsonly,both,ysonly) = sorted_difference xs ys in
       (xsonly,x::both,ysonly)
     |	n when n < 0 ->
       let (xsonly,both,ysonly) = sorted_difference xs (y::ys) in
       (x::xsonly,both,ysonly)
     |	n when n > 0 ->
       let (xsonly,both,ysonly) = sorted_difference (x::xs) ys in
       (xsonly,both,y::ysonly)
     |	_ -> failwith "not possible")

let rec remove v = function
    [] -> []
  | x :: xs -> if x = v then remove v xs else x :: remove v xs

let rec option_filter f = function
    [] -> []
  | x :: xs ->
    (match f x with
       Some y -> y :: (option_filter f xs)
     |	None -> option_filter f xs)

let rec option_partition f = function
    [] -> ([],[])
  | x :: xs ->
    let (some,none) = option_partition f xs in
    (match f x with
       Some y -> (y :: some, none)
     |	None -> (some, x :: none))

let rec split3 = function
    [] -> ([],[],[])
  | (a,b,c)::xs -> let (ar,br,cr) = split3 xs in (a::ar,b::br,c::cr)

let sum l = List.fold_left (function rest -> function n -> n + rest) 0 l
let sumfloat l =
  List.fold_left (function rest -> function n -> n +. rest) 0.0 l

let safe_hash_find table key default =
  try Hashtbl.find table key
  with Not_found -> let cell = default() in Hashtbl.add table key cell; cell

(* parent is a cell containing the alist *)
let safe_alist_find parent key default =
  try List.assoc key !parent
  with
    Not_found ->
    let cell = default() in parent := (key,cell) :: !parent; cell

(* assume list is nonempty *)
let get_min l =
  List.fold_left
    (function rest -> function x -> if x < rest then x else rest)
    (List.hd l) l

let get_max l =
  List.fold_left
    (function rest -> function x -> if x > rest then x else rest)
    (List.hd l) l

(* -------------------------------------------------------------------- *)
(* filename manipulation, for use by the parser *)

(* turn - into _ because - cannot occur in the middle of an identifier *)
let normalize s1 =
  let s = Bytes.of_string s1 in
  let ctr = ref 0 in
  String.iter
    (function c ->
       (match c with
          '-' -> Bytes.set s !ctr '_'
        |	_ -> ());
       ctr := !ctr + 1)
    s1;
  Bytes.to_string s

(* -------------------------------------------------------------------- *)
(* strings *)

let substring s1 s2 =
  let s1len = String.length s1 in
  let s2len = String.length s2 in
  let s1start = String.get s1 0 in (* don't bother checking for 0-len string *)
  let rec loop s2start =
    let s2start =
      try Some(String.index_from s2 s2start s1start)
      with Not_found -> None in
    match s2start with
      None -> false
    | Some s2start ->
      if s1len > s2len - s2start
      then false
      else
        let part = String.sub s2 s2start s1len in
        if part = s1
        then true
        else loop (s2start+1) in
  loop 0

let substring_index s1 s2 =
  let s1len = String.length s1 in
  let s2len = String.length s2 in
  let s1start = String.get s1 0 in (* don't bother checking for 0-len string *)
  let rec loop s2start =
    let s2start = String.index_from s2 s2start s1start in
    if s1len > s2len - s2start
    then raise Not_found
    else
      let part = String.sub s2 s2start s1len in
      if part = s1
      then s2start
      else loop (s2start+1) in
  loop 0

(* -------------------------------------------------------------------- *)

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
