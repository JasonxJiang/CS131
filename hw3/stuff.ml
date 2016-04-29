
let check x = 
	match x with 
		Some(s) -> s
	| None -> "hello" 

let l1 = [1;2;3]
let l2 = [1;2;3]
let x = match ((3,4),5) with ((a,b),c) -> a + b +c
let y = match ((3,4),5) with (a,(b,c)) -> 69 | ((a,b),c) -> a + b +c
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