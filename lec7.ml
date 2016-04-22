(*
Types 
-----


*)

let rec evalExpr (e:moexpr) (env:moenv) : movalue = 
 match e with 
  IntConst(i) -> IntVal(i)
 | Negate(e0) -> 
 	let v0 = evalExpr  