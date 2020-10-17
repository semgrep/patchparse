module CE = Ce
module Diff = Context_change

(* Filters for the changes in the interface *)

(* no good definitions for these yet... *)
let not_field = function
    Ast.SYMBOL([Ast.IDENT("",_)]) -> false
  | Ast.SYMBOL([Ast.IDENT(_)]) -> true
  | _ -> false

(* --------------------------------------------------------------------- *)

(* always takes the most general thing *)
let analyze_bottom f = function
    Diff.CC(change,context) ->
      let (change,cc) =
	(* the loop either ends at [], or ends at CCs, which can occur when
	   traversing the generalizations of a function call *)
	let rec loop change = function
	    [Diff.CG(change,context)] -> loop change context
	  | _ -> (change,Diff.CC(change,[])) in
	loop change context in
      if f change then Some cc else None
  | Diff.CG(change,context) -> failwith "cannot occur"

(* takes the most specific thing that matches the filter *)
let rec analyze_top f = function
    Diff.CG(change,context)
  | Diff.CC(change,context) as x ->
      if f change
      then Some x
      else 
	(match context with
	  [] -> None
	| [context] -> analyze_top f context
	| _ -> None)

(* --------------------------------------------------------------------- *)

(* changes in generic functions exported by the drivers *)
let exp_decl = function
    Ast.EXPR([Ast.SYMBOL(l)]) ->
      (match List.rev l with
	Ast.EXP(_,_)::_ -> true
      | _ -> false)
  | _ -> false

let is_exp = function
    Ast.SYMBOL([Ast.EXP(_,_)]) -> true
  | _ -> false

(* --------------------------------------------------------------------- *)

(* change in data structure layout: requires . or ->, field name stays the
same *)

let structure_access l =
  List.exists
    (function Ast.SYMOP(".",_) | Ast.SYMOP("->",_) -> true | _ -> false)
    l

let data_layout_change change =
  analyze_bottom
    (function mg ->
      match mg with
	CE.SYMCE(prims1,prims2) ->
	  Filter.diff_path_same_field mg &&
	  (structure_access prims1 || structure_access prims2)
      | _ -> false)
    change

(* public to private or vice versa *)

let public_private change =
  analyze_bottom
    (function mg -> Filter.make_public mg || Filter.make_private mg)
    change

(* --------------------------------------------------------------------- *)

(* changes in the protocol *)

(* functions added or removed only.  ordering changed will require more
work. *)

let calls_added_or_removed change =
  analyze_bottom (function mg -> Filter.addfn mg || Filter.dropfn mg) change

(* --------------------------------------------------------------------- *)

(* changes in the protocol *)

(* functions added or removed only.  ordering changed will require more
work. *)

let calls_change change =
  analyze_top (function mg -> Filter.any_change_in_call mg) change
