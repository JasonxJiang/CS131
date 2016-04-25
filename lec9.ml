let rec fact n = 
	match n with 
		0 -> 1
	| _ -> fact(n-1)

let factTR n = 
	let rec helper n res = 
		match n with 
		0-> res 
		| _ -> helper (n-1) (res *n)
	in helper n 1  


(* Tail call optimization 

A *tail call* is a function call that is the last operation 
done dynmically inside some function body. 


*)

let rec sumLstTR l = 
	let rec helper acc parL =
		match parL with 
		[] -> acc
		| h::t-> helper (acc+h) t 
	in helper 0 l 

