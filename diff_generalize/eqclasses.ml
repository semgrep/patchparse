module CE = Ce
module Config = Globals

(* We will have equivalence classes of changes and of individual - and +
strings, for the case where diff has inferred a meaningless pairing. *)

(* Idea: Traverse the changes in order from largest to smallest.  For each
size, see if every change made it into some tree.  For those that didn't,
add their children, which must have a smaller size, to the appropriate
place in the sorted list.  Not clear how to integrate processing of pairs
and of individual strings, so for the moment, we just do them
separately. *)

(* --------------------------------------------------------------------- *)
(* Get the size of the change at each root *)

let rec size_prim = function
    Ast.IDENT(s,_) -> 2 (* needs to be larger than EXP *)
  | Ast.CHAR(s,_) -> 1
  | Ast.INT(s,_) -> 1
  | Ast.STR (_) (*s*) -> 1
  | Ast.SYMOP(s,_) -> 1
  | Ast.ARRAY(exprlist,known) -> 1 + (size_exprlist exprlist)
  | Ast.PARENSYM(exprlist,known) -> 1 + (size_exprlist exprlist)
  | Ast.EXP(_,_) -> 1

and size_symbol sym = Aux.sum (List.map size_prim sym)

and size_expr = function
    Ast.SYMBOL(sym) -> 1 + (size_symbol sym)
  | Ast.EOP(s,_) -> 1
  | Ast.ASSIGN(lhs,op,rhs,known) ->
      1 + (size_expr lhs) + (size_exprlist rhs)
  | Ast.CALL(fn,args,known) | Ast.DECLARER(fn,args,known) ->
      if (List.for_all
	    (function Ast.CODE -> true | Ast.SEP(_) -> true | _ -> false)
	    args)
      then 1 + size_expr fn (* function with signature *)
      else if (List.exists (function Ast.CODE -> true | _ -> false) args)
      then 2 + size_expr fn (* function with signature *)
      else 2 + (size_expr fn) + (size_codelist args)
  | Ast.PROTOTYPE(fn,_,_,_,args,known) ->
      if (List.for_all
	    (function Ast.CODE -> true | Ast.SEP(_) -> true | _ -> false)
	    args)
      then 1 + size_expr fn (* function with signature *)
      else if (List.exists (function Ast.CODE -> true | _ -> false) args)
      then 2 + size_expr fn (* function with signature *)
      else 2 + (size_expr fn) + (size_codelist args)
  | Ast.STRUCT(codelist,known) -> 1 + (size_codelist codelist)

and size_code = function
    Ast.EXPR(exprlist) -> 1 + (size_exprlist exprlist)
  | Ast.SEP(s,_) -> 1
  | Ast.ARG(n) -> 1
  | _ -> failwith "not possible"

and size_exprlist l = 1 + Aux.sum (List.map size_expr l)
and size_codelist l = 1 + Aux.sum (List.map size_code l)
and size_codelistlist l = 1 + Aux.sum (List.map size_codelist l)

let size_ce = function
    CE.PRIMCE(p1,p2)    -> size_prim p1 + size_prim p2
  | CE.SYMCE(s1,s2)     -> size_symbol s1 + size_symbol s2
  | CE.EXPRCE(e1,e2)    -> size_expr e1 + size_expr e2
  | CE.EXPRLCE(el1,el2) -> size_exprlist el1 + size_exprlist el2
  | CE.CODECE(c1,c2)    -> size_code c1 + size_code c2
  | CE.CODELCE(cl1,cl2) -> size_codelist cl1 + size_codelist cl2

(* --------------------------------------------------------------------- *)
type change_table_type =
    (CE.ce,
     ((int (*version*) * string (*dir*)) * int (*sites*) *
	string list (*files*) * string list (*regions*) *
	CE.ce list (* same as key but without abstraction lines*)  ) list
    ) Hashtbl.t

(* the key is the size of the diff.ce of the context_change *)
type worklist_type = 
    (int, (Diff.context_change * int (*version*) * string (*dir*) *
	     string (*file*) * string (*region*)) list ref)
      Hashtbl.t
(* --------------------------------------------------------------------- *)

(* build worklist *)
(* just adds changes according to their size *)
(* int * string * string are version, pathname, and filename *)

let build_worklist table max_size size change
    (version, pathname, filename, region) =
  if size > !max_size then max_size := size;
  let cur = Aux.safe_hash_find table size (function _ -> ref []) in
  cur := (change,version,pathname,filename, region) :: !cur

let build_change_worklist change_worklist max_change_size ce
    (version, pathname, filename, region) =
  match ce with
    Diff.CC(change,context) | Diff.CG(change,context) ->
      build_worklist change_worklist max_change_size (size_ce change) ce
	(version, pathname, filename, region)


(* --------------------------------------------------------------------- *)
(* collect the set of all changes and strings *)
(* int and string are version and filename *)

let add_tmp table change context
    (version, pathname, filename, region, not_al_change) =
  let cur = Aux.safe_hash_find table change (function _ -> ref []) in
  cur := (context,version,pathname,filename, region, not_al_change) :: !cur

let vf_tmp_table =
  (Hashtbl.create(10) :
     ((int (*version*) * string (*pathname (dir) *)),
      int (*count*) ref * string (*filename*) list ref *
	string (*regions*) list ref *
	CE.ce (*not_al_change*) list ref)
     Hashtbl.t)

let tmp_table =
  (Hashtbl.create(1000) :
     (CE.ce,
      (Diff.context_change list * int * string * string * string *
	 CE.ce) list ref)
     Hashtbl.t)

let version_unused_table (* only filled in for git *) =
  (Hashtbl.create(1000) :
     (string (* version *), int ref (* unused tokens *)) Hashtbl.t)

let inc_version version =
  let version = Config.get_version version in
  let cell =
    try Hashtbl.find version_unused_table version
    with Not_found ->
      let cell = ref 0 in
      Hashtbl.add version_unused_table version cell; cell in
  cell := !cell + 1

let build_change_classes (change_worklist: worklist_type)
    (real_table: change_table_type) split
    size_fn size_fn1 max =
  let rec loop = function
      0 -> ()
    | n ->
	let entries =
	  try
	    let cell = Hashtbl.find change_worklist n in
	    let res = !cell in
	    cell := []; (* need to drop the data from the hash table *)
	    res
	  with Not_found -> [] in
	(* collect changes that are identical *)
	List.iter
	  (function (entry,version,pathname,filename, region) ->
	    let (change,context) = split entry in
	    add_tmp tmp_table (Ce_al.al_ce change) context
	      (version, pathname, filename, region, change))
	  entries;
	(* see if there are enough of them; if not recurse in the context *)
	Hashtbl.iter
	  (function change ->
	    function contexts ->
	      let local_contexts = !contexts in
	      contexts := [];
	      let sites = List.length local_contexts in
	      let files =
		Aux.union_list
		  (List.map
		     (function (_,version,_,filename,_,_) ->
		       (version,filename))
		     local_contexts) in
	      if
		sites >= !Config.same_threshold &&
		(List.length files) >= !Config.file_threshold
	      then
		begin (*
		  Printf.printf "files\n";
		  List.iter (function (version,filename) ->
		    Printf.printf "   %s %s\n"
		      (Hashtbl.find Config.version_table version)
		      filename)
		    files; *)
		  (* collect sites and counts *)
		  List.iter
		    (function (_,version,pathname,filename,region,
			       not_al_change) ->
		      let vf = (version,pathname) in
		      let (ct,fileset, regionset, not_al_changeset) =
			try Hashtbl.find vf_tmp_table vf
			with Not_found ->
			  let cell = (ref 0,ref [], ref[], ref[]) in
			  Hashtbl.add vf_tmp_table vf cell; cell in
		      ct := !ct + 1;
		      fileset := Aux.union [filename] !fileset;
		      regionset := Aux.union [region] !regionset;
                      not_al_changeset := not_al_change ::!not_al_changeset)
		    local_contexts;
		  let site_counts =
		    Hashtbl.fold
		      (function (version,pathname) ->
			function (ct,fileset, regionset, not_al_changeset) ->
			  function rest ->
			    ((version,pathname),!ct,
			     List.sort compare !fileset, 
                             List.sort compare !regionset,
			     !not_al_changeset)::rest)
		      vf_tmp_table [] in
		  Hashtbl.clear vf_tmp_table;
		  Hashtbl.add real_table change site_counts
		end
	      else
		List.iter
		  (function (context,version,pathname,filename, region,
			     not_al_change) ->
		    if context = []
		    then inc_version version
		    else
		      let sizes = List.map size_fn context in
		      if List.exists (function n1 -> n1 >= n) sizes
		      then
			begin
			  Printf.printf "parent %d\n%s\n" n
			    (Ce_unparse.ce2c change);
			  Printf.printf "children %s\n%s\n"
			    (String.concat " " (List.map string_of_int sizes))
			    (String.concat "\nand\n"
			       (List.map
				  (function
				      Diff.CC(change,_) | Diff.CG(change,_) ->
					Ce_unparse.ce2c change)
				  context));
			  Printf.printf
			    "a child has the same size as its parent %d %s"
			    version filename
			    (* do nothing *)
			end
		      else
		      List.iter2
			(function size ->
			  function change ->
			    build_worklist change_worklist (ref 0) size change
			      (version, pathname, filename, region))
			sizes context)
		  local_contexts)
	  tmp_table;
	Hashtbl.clear tmp_table;
	loop (n-1) in
  loop (!max)
    
let build_context_change_classes change_worklist change_table max_change_size =
  build_change_classes change_worklist change_table
    (function Diff.CC(change,context) | Diff.CG(change,context) ->
      (change,context))
    (function Diff.CC(change,context) | Diff.CG(change,context) ->
      size_ce change)
    size_ce
    max_change_size
    
(* --------------------------------------------------------------------- *)
(* Output *)
    
let print_change_table change_table =
  Hashtbl.iter
    (function change ->
      function site_counts ->
	Printf.printf "%d: %s\n"
	  (Aux.sum (List.map (function (_,ct,_,_,_) -> ct) site_counts))
	  (Ce_unparse.ce2c change))
    change_table
    
(* --------------------------------------------------------------------- *)
(* Entry point *)
    
let eqworklists worklist max_change_size change
    (version, pathname, filename, region) =
  build_change_worklist worklist max_change_size
    change (version, pathname, filename, region)
    
let eqclasses worklist (change_table: change_table_type) max_change_size =
  build_context_change_classes worklist change_table max_change_size (*;
  if !Config.notex then print_change_table change_table *)
    
    
let for_debug  (big_worklist: worklist_type) = ()
    
