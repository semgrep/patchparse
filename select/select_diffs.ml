module CE = Ce
module Config = Globals
module CC = Change_tree

(* Presumably, each CC or CG represents a single change, with children
   various abstractions of those changes or subchanges.  This permits only
   filtering on the root. *)

let constants_only = function
    CE.PRIMCE(Ast.IDENT(s1,_),Ast.IDENT(s2,_))
    when
      s1 = String.uppercase_ascii s1 &&
      s2 = String.uppercase_ascii s2 -> true
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
            List.fold_left
              (fun prev cur ->
                 match cur with
                   CC.CG(change,_) | CC.CC(change,_) ->
                   if filter change
                   then cur :: prev
                   else prev)
              [] changelist in
          let filtered = List.rev filtered in
          if filtered = []
          then []
          else [(filtered,info)])
       l)
