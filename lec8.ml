(*Exceptions *) 

(* lookup a key's *)

let rec lookup k l = 
 match l with 
  [] -> None 
 | (k', v')::rest -> if k=k' then Some v' else lookup k rest 

 (*This style of error signaling doesn't compose well *)

 let lookupAndDouble k l = 
 	match (lookup k l) with 
 		None -> None 
 	| Some v -> Some (v*2) 

(*)	match ks with 
		[] -> Some [] 
	| k::rest -> match (lookupAll rest l) with 
		None -> None 
	| Some resList -> 
		match (lookup k l) with 
			None -> None 
		| Some v -> Some(v::resList)
*)
(* execptions let you separate error handling from ordinary program logic *)

exception Not_found

let rec lookupE k l = 
	match l with 
		[] -> raise Not_found
	| (k',v')::rest -> if k=k' then v' else lookupE k rest 

let rec lookupAll ks l = 
	List.map(fun key -> lookup key l) ks   
(* try e with p1 -> e1 | ... | pn -> en *)

let rec safeLookupAllE ks l = 
	try 
		safeLookupAllE ks l 
	with 
		Not_found -> [] 


(* Parametric Polymorphism *)
let rec length l = 
	match l with 
		[] -> 0 
	| _::t -> 1 + length t 

(* 'a is a *type varaibel *
	can think of it as an extra parameter to the function 

	lenth <int> [1;2;3]
	length <string> ["hi";"bye"]
	(not actual syntax)

	the type instantiation (passing the implicit type parameter) 
	happens at compimle time 

	1 piece of code, which can be safely passed arguments of many different types 
*)	

(* don't confuse parametric polymorphism with static overloading 
	static overlaoding: many different pieces of code,all with the same name, 
	one per type that you care about 
	
	e.g., + for ints and + for floats in (C, Java)

	Unlike parametric polymorphism, static overloading is a syntactic convenience: use the 
	same name for different things

	= is defined as polymorophic but is really treated as overloaded since it hasn't been 
	defined for all types e.g. comparing functions 	

	Parametric Polymorphism 
	You typecheck once during compile. It looks up the type it expects and checks if the passed in 
	parameters matches 
	Templates don't have any guarentees you must recheck its type everytime you invoke it 
	whereas polymorphism a check for 
*)



let maybeSwap (x,y) b = 
		if b 
		then (y,x) 
		else (x,y);;
(*note although the types may diverge since x and y are different, you are not allowed to do that 
thus x and y must be of the same type *)





(* Modues in OCaml *)

module type SET = sig 
	type 'a t 
	val emptyset : 'a t
	val member : 'a t -> 'a t -> bool 
	val add : 'a -> 'a t -> 'a t 
	val union : 'a t-> 'a t -> 'a t 
	(* if you want a function to be private just don't show it in the sig *)
end 

module Set : SET = struct 

	type 'a t = 'a list 

	let emptyset = [] 

	let rec  member =
  	fun x s ->
  		match s with 
			[] -> false
		|	h::t -> if x=h then true else (member x t);; 
 

	let add =
  		fun x s ->
  			if member x s then s else x::s;;



	let rec union =
  		fun s1 s2 ->
  			match s1 with 
  				[] -> s2
			|	h::t -> if (member h s2) then union t s2 else h::(union t s2);;
end

(* need a way to separate *interface* from *implementation*: 
	implementation: a list
	interface: a set 

	ensure that sets can't have duplicates 
	let me upgrade my internal implementation without breaking callers 
*)

