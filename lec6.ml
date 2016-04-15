(* binary trees *)

type btree = Leaf | Node of btree * int * btree 

let rec height t = 
	match t with 
	Leaf -> 1;
	| Node(l, _, r) -> 1 + max(height l) (height r);; 

(* Insert into a binary search tree *)
let rec insert n t = 
	match t with 
		Leaf -> Node(Leaf, n, Leaf)
	| Node(l, value, r) -> if (n < value) then Node(insert n l,value, r) 
							else if  (n > value) then Node(l,value, insert n r)
							else t;;  

(* Variable scoping *)

(*

How can we declare variables?

*top-level let:
	let x = 5;;
	scope: the rest of the program 

*let expression:
	let x = 5 in x + 2
	let x = e1 in e1 
	scope e2 


*pattern matching:
	match [1;2;3] with 
		h::t-> e1 
	| p2 -> e2 
	| pn -> en  
	scope: of h and t: e1 	

*function parameters 
	function x -> x +1
	function x -> e 
	scope of x:e

	static scoping or lexical scoping 
	at compile time you can determine, for each variable usage,
	which variage declaration it refers to. 

	*nearest enclosing definition in the program test* 
	*)

let x = 45;; 
let f = function y -> x + y;;
f 3;;

let x = 12;; 
x;;
f 3;;

(*
an environment is a representation of the scope:
	-a map from variables to values 

{} 
	let x = 45;; 
{x:45}
	let f = function y -> x+y;; 
{x:45, f:((function y ->x+y), {x:45})}
	
	f 3
	-lookup f in the environment
	-evaluate f in the environment 
	(function y -> x +y) 3
	-get the function's lexical/static environment
	{x:45}
	-substitute actual args for formal paraeters 
	-
	{x:45, f:(function y ->x=Y)}, f:(function y -> x=Y), y:3)
		x+y 
	returns 48 




*)

let add = fun x y -> x+y;;

(*

	let add = fun x y -> x+y
	{add: (fun x y -> x +y, {})}

	let addTwo = add 2
	what happens?
	1. lookup add in the environment 
	2. execute it in add's static environment {} 
		bind x to 2: {x:2}
		execute function body: function y -> x + y 
	{add :.., addTwo : (function y->x+y, {x:2})}

key point: a variable's *lifetime" is different from its scope:
	a variable can be live, because other code refers to it,
	even if its out of scope

*)