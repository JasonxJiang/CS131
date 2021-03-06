
exception ImplementMe

(* Problem 1 *)
            
let rec  (member : 'a -> 'a list -> bool) =
  fun x s ->
  match s with 
	[] -> false
|	h::t -> if x=h then true else (member x t);; 
 

let (add : 'a -> 'a list -> 'a list) =
  fun x s ->
  	if member x s then s else x::s;;



let rec (union : 'a list -> 'a list -> 'a list) =
  fun s1 s2 ->
  match s1 with 
  	[] -> s2
|	h::t -> if (member h s2) then union t s2 else h::(union t s2);;



let rec (fastUnion : 'a list -> 'a list -> 'a list) =
  fun s1 s2 ->
  match s1 with 
  	[] -> s2 
|	h1::t1 -> match s2 with 
				[] -> h1::(fastUnion t1 s2)
			|	h2::t2 -> if (h1<h2) then 
					h1::(fastUnion t1 s2)
				else if (h2<h1) then
					h2::(fastUnion s1 t2)
				else 
					h1::(fastUnion t1 t2);;


					
                
let (intersection : 'a list -> 'a list -> 'a list) =
  fun s1 s2 -> match s2 with 
  		[] -> []
	| 	h::t -> List.filter((fun x -> (member x s1))) s2;;



let rec (setify : 'a list -> 'a list) =
  fun l -> match l with 
  		[] -> []
	|	h::t -> if (member h t) then setify t else h::(setify t);;


let rec (powerset : 'a list -> 'a list list) = 
	fun s -> match s with 
		[] -> [[]]
	|	h::t -> let l = powerset t in 
			l @ (List.map(fun x -> h::x) l);;

        
(* Problem 2 *)        


let rec (partition : ('a -> bool) -> 'a list -> 'a list * 'a list) =
	fun x s -> match s with 
		[] -> ([], [])
	|	h::t -> let (y, z) = (partition x t) in
			if (x h) then (h::y, z) else (y, h::z);;


let rec (whle : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a) =
  fun p f x -> if (p x) then (whle p f (f x)) else x;;



  
let rec (pow : int -> ('a -> 'a) -> ('a -> 'a)) =
	fun n f -> match n with 
				0 -> (fun x -> x)
			|	_	-> (fun x -> f (pow (n-1) f x));;




























