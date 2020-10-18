open Ce

let process_ce combiner mapper primfn exprfn codefn = function
  | PRIMCE(prim1, prim2) ->
    combiner (primfn prim1, primfn prim2)
  | SYMCE(symbol1,  symbol2) ->
    combiner (mapper (List.map primfn symbol1),
              mapper (List.map primfn symbol2))
  | EXPRCE(expr1,  expr2) ->
    combiner (exprfn expr1, exprfn expr2)
  | EXPRLCE(expr_list1 , expr_list2) ->
    combiner (mapper (List.map exprfn expr_list1),
              mapper (List.map exprfn expr_list2))
  | CODECE(code1, code2) ->
    combiner (codefn code1, codefn code2)
  | CODELCE(code_list1, code_list2) ->
    combiner (mapper (List.map codefn code_list1),
              mapper (List.map codefn code_list2))

let rebuild_ce primfn exprfn codefn = function
  | PRIMCE(prim1, prim2) ->
    let prim1 = primfn prim1 in
    let prim2 = primfn prim2 in
    PRIMCE(prim1, prim2) 
  | SYMCE(symbol1, symbol2) ->
    let symbol1 = List.map primfn symbol1 in
    let symbol2 = List.map primfn symbol2 in
    SYMCE(symbol1, symbol2)
  | EXPRCE(expr1,  expr2) ->
    let expr1 = exprfn expr1 in
    let expr2 = exprfn expr2 in
    EXPRCE(expr1, expr2)
  | EXPRLCE(expr_list1 , expr_list2) ->
    let expr_list1 = List.map exprfn expr_list1 in
    let expr_list2 = List.map exprfn expr_list2 in
    EXPRLCE(expr_list1, expr_list2)
  | CODECE(code1, code2) ->
    let code1 = codefn code1 in
    let code2 = codefn code2 in
    CODECE(code1, code2)
  | CODELCE(code_list1, code_list2) ->
    let code_list1 = List.map codefn code_list1 in
    let code_list2 = List.map codefn code_list2 in
    CODELCE(code_list1, code_list2)

