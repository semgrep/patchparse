module CE = Ce

(* Presumably, each CC or CG represents a single change, with children
various abstractions of those changes or subchanges.  This permits only
filtering on the root. *)

let constants_only = function
    CE.PRIMCE(Ast.IDENT(s1,_),Ast.IDENT(s2,_))
      when s1 = String.uppercase s1 && s2 = String.uppercase s2 -> true
  | _ -> false

let anything (_ : CE.ce) = true

let select_diffs l =
  let filter =
    match !Config.filter with
      Config.ConstantsOnly -> constants_only
    | Config.Anything -> anything in
  List.concat
    (List.map
       (function (changelist,info) ->
	 let filtered =
	   List.filter
	     (function Diff.CG(change,_) | Diff.CC(change,_) -> filter change)
	     changelist in
	 if filtered = []
	 then []
	 else [(filtered,info)])
       l)
