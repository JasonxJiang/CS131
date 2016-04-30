
let check x = 
	match x with 
		Some(s) -> s
	| None -> "hello" 

let l1 = [1;2;3]
let l2 = [1;2;3]
let x = match ((3,4),5) with ((a,b),c) -> a + b +c
(*let y = match ((3,4),5) with (a,(b,c)) -> 69 | ((a,b),c) -> a + b +c*)

let y = Data("Leaf", Some(Tuple([IntConst(5);IntConst(6)])))
let e = Env.empty_env()
let x = Data("Leaf", None)
let leafpat1 = (DataPat("Leaf", Some(TuplePat([IntPat(5);IntPat(6)]))))
(*let x' = TuplePat([DataPat("Leaf", TuplePat([IntVal(a);IntVal(b)]))])*)
let y' = DataPat("Leaf", None)
(*let testList = [TuplePat([DataPat("Leaf", TuplePat([IntVal(a);IntVal(b)]))]); TuplePat([DataPat("Leaf",None);WildcardPat])]

*)
let testMo = evalExpr y e

let testEmpty = Data("Leaf", None)
let testMoEmpty = evalExpr testEmpty e
let testList = [(leafpat1, IntConst(0));(y', IntConst(1))]

let dcFalse = TuplePat([WildcardPat;BoolPat(false)])
let leafTrue = TuplePat([DataPat("Leaf", Some(TuplePat([IntPat(5);IntPat(6)])));WildcardPat])
let matchexp = Tuple([Data("Leaf", None);BoolConst(false)])
let matchList = [(dcFalse, IntConst(0));(leafTrue,IntConst(1))]

(*let m = function a -> function b -> 
		match (a,b) with 	
		(true,1) -> 1 
	| (Leaf(a,b),_) -> a + b 
	| (_,false) -> 2

Leaf(5,6) true 

List.fold_left 

match a with 
	Leaf(a) -> Leaf(c)

pat = Data("leaf", varPat)
movalue = a  *)