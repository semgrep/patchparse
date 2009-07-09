(* groups same things and different things.  Within a list of same things
or different things, there are sublists of interesting things (always
length 1), and of boring things (any length greater than 0) *)


(*
let (=|=) : int    -> int    -> bool = (=)
let (=<=) : char   -> char   -> bool = (=)
let (=$=) : string -> string -> bool = (=)
let (=:=) : bool   -> bool   -> bool = (=)
let (=*=) = (=)
let (=) = (=|=)
*)

type 'data annotated =
    Boring of 'data
  | Interesting of 'data

(* ---------------------------------------------------------------------- *)
(* The following is organized such that every interesting element has a
sequence of boring predecessors and successors.  For example, if the
original list has an interesting element at the beginning or end, then
there will be an empty boring element before or after it respectively. *)

type 'data collected =
    CBoring of 'data list
  | CInteresting of 'data

let parse_data l =
  let rec parse_interesting = function
      [] -> []
    | (Interesting(data))::rest ->
	let (boring,rest) = parse_boring rest in
	CInteresting(data) :: CBoring(boring) :: rest
    | _ -> failwith "not possible"
  and parse_boring = function 
      (Boring(data))::rest ->
	let (boring,rest) = parse_boring rest in
	(data::boring,rest)
    | rest -> ([], parse_interesting rest) in
  let (boring,rest) = parse_boring l in
  CBoring(boring) :: rest

(* ---------------------------------------------------------------------- *)
(* Insert links, such that the interesting and boring elements point to
their predecessors and successors, which may be absent in the boring
case. *)

type 'data connected_list =
    I of 'data *
	'data connected_list option ref * 'data connected_list option ref
  | B of 'data list ref *
	'data connected_list option ref * 'data connected_list option ref

(* bdata is boring data in the case where there are no interesting elements *)
let rec collect_interesting bdata = function
    None -> (bdata,[])
  | Some (B(data,left,right)) -> collect_interesting !data !right
  | Some (I(data,left,right)) ->
      let (_,rest) = collect_interesting [] !right in
      let left_data =
	match !left with
	  Some(B(data,left,right)) -> Some data
	| _ -> None in
      let right_data =
	match !right with
	  Some(B(data,left,right)) -> Some data
	| _ -> None in
      ([],(data,left_data,right_data) :: rest)

let reparse_data l =
  let make_entries l =
    List.map
      (function
	  CBoring(data) -> B(ref data,ref None,ref None)
	| CInteresting(data) -> I(data,ref None,ref None))
      l in
  let rec connect_left_right prev = function
      [] -> None
    | ((B(data,left,right)) as x)::rest ->
	left := prev;
	let aft = connect_left_right (Some x) rest in
	right := aft;
	Some x
    | ((I(data,left,right)) as x)::rest ->
	left := prev;
	let aft = connect_left_right (Some x) rest in
	right := aft;
	Some x in
  collect_interesting [] (connect_left_right None (make_entries l))

(* ---------------------------------------------------------------------- *)

type 'data result =
    Same of 'data list list * 'data list list
  | Similar of 'data list list * 'data list list
  | Different of 'data list list * 'data list list
  | Boring_data of 'data list list * 'data list list

let index comparer f x l =
  let rec loop = function
      [] -> raise Not_found
    | (y::rest) -> if comparer (x,f y) then 0 else 1 + loop rest in
  try Some(loop l) with Not_found -> None

let split l n =
  let rec loop = function
      (0,((x,left,right) as front)::rest) -> ([],left,front::rest)
    | (n,x::rest) ->
	let (pref,left,rest) = loop (n-1, rest) in
	(x::pref,left,rest)
    | _ -> failwith "not possible: split" in
  loop (n,l)

let rec common_prefixes comparer = function
    ([],r) -> ([],[],r)
  | (r,[]) -> ([],r,[])
  | (((x::rest1) as all1),((y::rest2) as all2)) ->
      if comparer (x, y)
      then
	let (pref,rest1,rest2) = common_prefixes comparer (rest1,rest2) in
	(x::pref,rest1,rest2)
      else ([],all1,all2)

let rec unary_prefix unary = function
    [] -> ([],[])
  | x::xs when unary x ->
      let (pref,rest) = unary_prefix unary xs in (x::pref,rest)
  | rest -> ([],rest)

let consume_prefix comparer right1 right2 =
  match (right1,right2) with
    (None,_) -> []
  | (_,None) -> []
  | (Some(datacell1),Some(datacell2)) ->
      let (pref,rest1,rest2) = common_prefixes comparer (!datacell1,!datacell2) in
      datacell1 := rest1;
      datacell2 := rest2;
      pref

let consume_suffix comparer unary right1 right2 =
  match (right1,right2) with
    (None,None) -> ([],[])
  | (Some(datacell1),None) ->
      let (unary1,rest1) = unary_prefix unary (List.rev !datacell1) in
      datacell1 := List.rev rest1;
      (List.rev unary1,[])
  | (None,Some(datacell2)) ->
      let (unary2,rest2) = unary_prefix unary (List.rev !datacell2) in
      datacell2 := List.rev rest2;
      ([],List.rev unary2)
  | (Some(datacell1),Some(datacell2)) ->
      let (pref,rest1,rest2) =
	common_prefixes comparer (List.rev !datacell1, List.rev !datacell2) in
      let (unary1,rest1) = unary_prefix unary rest1 in
      let (unary2,rest2) = unary_prefix unary rest2 in
      let pref = List.rev pref in
      let unary1 = List.rev unary1 in
      let unary2 = List.rev unary2 in
      datacell1 := List.rev rest1;
      datacell2 := List.rev rest2;
      (unary1@pref,unary2@pref)

let rec prefix_fn fn = function
    [] -> ([],[])
  | (x::rest as all) ->
      if fn x
      then
	let (pref,rest) = prefix_fn fn rest in
	(x::pref,rest)
      else ([],all)

let consume_prefix_fn fn = function
    None ->  []
  | Some right ->
      let (pref,rest) = prefix_fn fn !right in
      right := rest;
      pref

let consume_suffix_fn fn = function
    None ->  []
  | Some left ->
      let (pref,rest) = prefix_fn fn (List.rev !left) in
      left := (List.rev rest);
      (List.rev pref)

let get_boring = function
    None -> []
  | Some(datacell) -> let v = !datacell in datacell := []; v

(* for when we want to just drop empty values *)
let listify = function
    [] -> []
  | v -> [v]

let rec flatten boring_context left l = (* collect all *)
  let rec loop = function
      [] -> []
    | [(data,left,right)] ->
	let boring = listify(consume_prefix_fn boring_context right) in
	[[data]] @ boring
    | (data,left,right)::rest ->
      let boring = listify(get_boring right) in
      [[data]] @ boring @ (loop rest) in
  let boring = listify(consume_suffix_fn boring_context left) in
  boring @ loop l

let update cell v =
  match cell with
    Some x -> x := v
  | _ -> failwith "not possible"

let insert_boring cell1 cell2 =
  match (get_boring cell1,get_boring cell2) with
    ([],[]) -> []
  | (b1,b2) -> [Boring_data(listify b1,listify b2)]

let rec align_interesting compare_current start comparers unary boring_context = function
    ([],[]) -> []
  | ((((data1,left1,right1)::rest1) as all1),[]) ->
      let flat1 = flatten boring_context left1 all1 in
      (insert_boring left1 None) @
      Different(flat1,[]) ::
      (insert_boring right1 None)
  | ([],(((data2,left2,right2)::rest2) as all2)) ->
      let flat2 = flatten boring_context left2 all2 in
      (insert_boring None left2) @
      Different([],flat2) ::
      (insert_boring None right2)
  | (((data1,left1,right1)::rest1),
     ((data2,left2,right2)::rest2))
    when compare_current(data1,data2) ->
      if start
      then
       (* data really the same *)
       let (before1,before2) =
	 consume_suffix (List.hd comparers) unary left1 left2 in
       let before1 = listify before1 in
       let before2 = listify before2 in
       let after = listify(consume_prefix (List.hd comparers) right1 right2) in
       let rest = (* needed sequence, neighbors take what they want *)
	 align_interesting (List.hd comparers) true comparers unary
	   boring_context (rest1,rest2) in
       let cur = Same(before1@[[data1]]@after,before2@[[data2]]@after) in
       (insert_boring left1 left2) @
       cur ::
       (insert_boring right1 right2) @
       rest
      else
       (* data only similar *)
       let (aftersbefore1,aftersbefore2) =
	 consume_suffix (List.hd comparers) unary right1 right2 in
       let (after1,after2) =
	 match (consume_prefix_fn boring_context right1,
		consume_prefix_fn boring_context right2) with
	   ([],[]) -> ([],[])
	 | (b1,b2) -> ([b1],[b2]) in
       update right1 aftersbefore1;
       update right2 aftersbefore2;
       let (before1,before2) =
	 match (consume_suffix_fn boring_context left1,
		consume_suffix_fn boring_context left2) with
	   ([],[]) -> ([],[])
	 | (b1,b2) -> ([b1],[b2]) in
       let rest =
	 align_interesting (List.hd comparers) true comparers unary
	   boring_context (rest1,rest2) in
       let cur = Similar(before1@[[data1]]@after1,before2@[[data2]]@after2) in
       (insert_boring left1 left2) @
       cur ::
       (insert_boring right1 right2) @
       rest
  | (((((data1,left1,right1) as x)::rest1) as all1),
     ((((data2,left2,right2) as y)::rest2) as all2)) ->
       (* data different *)
       let get_data (data,left,right) = data in
       let pickx comparer start xindex =
	 let (pref2(*<xindex*),rightp2,rest2(*>=xindex*)) =
	   split all2 xindex in
	 let rest = (* needed sequence *)
	   align_interesting comparer start comparers unary
	     boring_context (all1,rest2) in
	 let cur = Different([],flatten boring_context left2 pref2) in
	 (insert_boring left1 left2) @
	 cur ::
	 (insert_boring right1 right2) @
	 rest in
       let picky comparer start yindex =
	 let (pref1(*<yindex*),rightp1,rest1(*>=yindex*)) =
	   split all1 yindex in
	 let rest = (* needed sequence *)
	   align_interesting comparer start comparers unary
	     boring_context (rest1,all2) in
	 let cur = Different(flatten boring_context left1 pref1,[]) in
	 (insert_boring left1 left2) @
	 cur ::
	 (insert_boring right1 right2) @
	 rest in
       let rec compare_loop start = function
	   [] -> (* ran out of comparers, move on *)
	     let rest = (*needed sequence*)
	       align_interesting (List.hd comparers) true comparers unary
		 boring_context (rest1,rest2) in
	     let flat1 = flatten boring_context left1 [x] in
	     let flat2 = flatten boring_context left2 [y] in
	     (insert_boring left1 left2) @
	     Different(flat1,flat2) ::
	     (insert_boring right1 right2) @
	     rest
	 | comparer::others ->
	     (match (index comparer get_data data1 all2,
		    index comparer get_data data2 all1) with
	       (None,None) -> compare_loop false others
	     | (Some xindex,None) -> pickx comparer start xindex
	     | (None, Some yindex) -> picky comparer start yindex
	     | (Some xindex,Some yindex) ->
		 if xindex < yindex
		 then pickx comparer start xindex
		 else picky comparer start yindex) in
       compare_loop true comparers

let start_align_interesting comparers unary boring_context l1 l2 =
  match (List.rev l1, List.rev l2) with
    ((data1,left1,right1)::_,(data2,left2,right2)::_) ->
      (* at end, want to pretend there is a same elem
      at the end to eat up any common boring operators *)
      let (endstuff,_) =
	consume_suffix (List.hd comparers) (function _ -> false)
	  right1 right2 in
      (match listify endstuff with
	[] ->
	  align_interesting (List.hd comparers) true comparers unary
	    boring_context (l1,l2)
      |	after ->
	  (align_interesting (List.hd comparers) true comparers unary
	     boring_context (l1,l2))@
	  [Same(after,after)])
  | _ ->
      align_interesting (List.hd comparers) true comparers unary
	boring_context (l1,l2)

(* ---------------------------------------------------------------------- *)

(* We don't want to leave boring things all by themselves.  Once things are
aligned, it is no problem to add different only boring data onto the front
of same data.  Note that boring data is always clumped together in a single
list. *)

let all_boring boring = function
    [] -> Some [[]]
  | [l] as x when List.for_all boring l -> Some x
  | _ -> None

(* Same things always line up, so can be concatenated with each other.
Same for Similar, but can't be concatenated with Same.
Different things don't line up, so can't be concatenated with Same things *)

let rec combine_same_diff = function
    [] -> []
  | Same(dataa,datab)::rest ->
      (match combine_same_diff rest with
	Same(data1,data2)::rest -> Same(dataa@data1,datab@data2)::rest
      |	rest -> Same(dataa,datab)::rest)
  | Similar(dataa,datab)::rest ->
      (match combine_same_diff rest with
	Similar(data1,data2)::rest -> Similar(dataa@data1,datab@data2)::rest
      |	rest -> Similar(dataa,datab)::rest)
  | Different(data1,data2)::rest ->
      (match combine_same_diff rest with
	Different(data1a,data2a)::rest ->
	  Different(data1@data1a,data2@data2a)::rest
      |	rest -> Different(data1,data2)::rest)
  | Boring_data(data1,data2)::rest ->
      (match combine_same_diff rest with
	Boring_data(data1a,data2a)::rest ->
	  Boring_data(data1@data1a,data2@data2a)::rest
      |	rest -> Boring_data(data1,data2)::rest)

let allempty l = 
  List.for_all
    (function l -> List.for_all (function [] -> true | _ -> false) l)
    l

let flatten_result bdata res =
  let res = combine_same_diff res in
  let rec loop = function
      [] -> ([],[])
    | Same(data1,data2)::rest ->
	let (left,right) = loop rest in
	(data1 :: left, data2 :: right)
    | Similar(data1,data2)::rest ->
	let (left,right) = loop rest in
	(data1 :: left, data2 :: right)
    | Different(data1,data2)::rest ->
	let (left,right) = loop rest in
	(data1 :: left, data2 :: right)
    | Boring_data(data1,data2)::rest ->
	let (left,right) = loop rest in
	(data1 :: left, data2 :: right) in
  match (bdata,loop res) with
    (([],[]),res) -> res
  | ((l,r),([],[])) -> ([[l]],[[r]])
  | ((l,[]),(left,right)) when allempty left -> ([[l]],right)
  | (([],r),(left,right)) when allempty right -> (left,[[r]])
  | _ -> failwith "flatten_result: not possible"

(* ---------------------------------------------------------------------- *)
(* print result of flatten_result *)

let unparse inner2string (l1,l2) =
  let print_list f l =
    Printf.sprintf "[%s]" (String.concat ";" (List.map f l)) in
  let fn =
    print_list (print_list (print_list inner2string)) in
  Printf.printf "%s\n%s\n\n" (fn l1) (fn l2)

(* ---------------------------------------------------------------------- *)
(* Entry point *)

let top boring boring_context comparers unary (l1,l2) =
  let l1 =
    List.map
      (function x -> if boring x then Boring x else Interesting x)
      l1 in
  let l2 =
    List.map
      (function x -> if boring x then Boring x else Interesting x)
      l2 in
  let (bdata1,interesting1) = reparse_data(parse_data l1) in
  let (bdata2,interesting2) = reparse_data(parse_data l2) in
  flatten_result
    (* boring things that want a context *)
    (bdata1,bdata2)
    (start_align_interesting comparers unary
       boring_context (* boring things that want a context *)
       interesting1 interesting2)

(* ---------------------------------------------------------------------- *)
(* Uncomment to run standalone *)

(*
let parse_string s =
  let res = ref [] in
  String.iter (function c -> res := (Printf.sprintf "%c" c) :: !res) s;
  List.rev !res

let boring_char = function
    "+" | "-" | "*" | "/" | " " | "!" -> true
  | _ -> false

let test1 = (parse_string "x+-y/z", parse_string "m+n/z")
let test2 = (parse_string "x+y/z", parse_string "m+n/z")
let test3 = (parse_string "a+b+x+y/z", parse_string "a+b+m+n/z")
let test4 = (parse_string "a+b+x+y/z", parse_string "a-b+m+n/z")
let test5 = (parse_string "a+b+x+r+t+y/z", parse_string "a-b+m+n/z")
let test6 = (parse_string "a+b+c+d", parse_string "a+d")
let test7 = (parse_string "aaa+b+c+d", parse_string "a+d")
let test8 = (parse_string "++++", parse_string "---")
let test9 = (parse_string "++++", parse_string "---d")
let test10 = (parse_string "x", parse_string "y")
let test11 = (parse_string "x+a+", parse_string "y+z+")
let test12 = (parse_string "b+c+F+d+e+x+y+z", parse_string "b+d+G+e+m+n+z")
let test13 = (parse_string "xy", parse_string "y")
let test14 = (parse_string "++y", parse_string "+y")
let test15 = (parse_string "+x", parse_string "+y")
let test16 = (parse_string "a+x+y+z", parse_string "a+x-y+z")
let test17 = (parse_string "+x", parse_string "-y")
let test18 = (parse_string "d+x", parse_string "+++y+d+x")
let test19 = (parse_string "c+d", parse_string "d")
let test20 = (parse_string "a+c", parse_string "a-d")
let test21 = (parse_string "a+x", parse_string "b-y")
let test22 = (parse_string "abc", parse_string "ac")

let falsefn _ = true

let call_top =
  top boring_char falsefn [(function (x,y) -> x = y)] (function x -> x = "!")

let call_unparse = unparse (function inner_elem -> inner_elem)

let main _ =
  Printf.printf "test1\n";
  call_unparse(call_top test1);
  Printf.printf "test2\n";
  call_unparse(call_top test2);
  Printf.printf "test3\n";
  call_unparse(call_top test3);
  Printf.printf "test4\n";
  call_unparse(call_top test4);
  Printf.printf "test5\n";
  call_unparse(call_top test5);
  Printf.printf "test6\n";
  call_unparse(call_top test6);
  Printf.printf "test7\n";
  call_unparse(call_top test7);
  Printf.printf "test8\n";
  call_unparse(call_top test8);
  Printf.printf "test9\n";
  call_unparse(call_top test9);
  Printf.printf "test10\n";
  call_unparse(call_top test10);
  Printf.printf "test11\n";
  call_unparse(call_top test11);
  Printf.printf "test12\n";
  call_unparse(call_top test12);
  Printf.printf "test13\n";
  call_unparse(call_top test13);
  Printf.printf "test14\n";
  call_unparse(call_top test14);
  Printf.printf "test15\n";
  call_unparse(call_top test15);
  Printf.printf "test16\n";
  call_unparse(call_top test16);
  Printf.printf "test17\n";
  call_unparse(call_top test17);
  Printf.printf "test18\n";
  call_unparse(call_top test18);
  Printf.printf "test19\n";
  call_unparse(call_top test19);
  Printf.printf "test20\n";
  call_unparse(call_top test20);
  Printf.printf "test21\n";
  call_unparse(call_top test21);
  Printf.printf "test22\n";
  call_unparse(call_top test22)

let _ = main ()
*)
