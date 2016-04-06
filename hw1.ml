
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
					
(*testing*)
                
(*let (intersection : 'a list -> 'a list -> 'a list) =
  raise ImplementMe
                
let (setify : 'a list -> 'a list) =
  raise ImplementMe
         
let rec (powerset : 'a list -> 'a list list) =
  raise ImplementMe

        
(* Problem 2 *)        
        
let (partition : ('a -> bool) -> 'a list -> 'a list * 'a list) =
  raise ImplementMe

let (whle : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a) =
  raise ImplementMe
                                    
let (pow : int -> ('a -> 'a) -> ('a -> 'a)) =
  raise ImplementMe
 *)                    
