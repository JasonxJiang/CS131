let rec rev (l :'a list) : a' list = 
	let rec helper (acc :'a list) (t :'a list)  = match t with 
		[] ->	acc 
	|	hh::tt -> helper (hh::acc) tt 
		in helper [] l;; 

type peano = Zero | Succ of peano;;

let peanoToInt m