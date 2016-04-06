
exception ImplementMe

(* Problem 1 *)
            
let rec  (member : 'a -> 'a list -> bool) =
  fun x s ->
  match s with 
	[] -> false
|	h::t -> if x=h then true else (member x t);; 

(*let (add : 'a -> 'a list -> 'a list) =
  raise ImplementMe

let (union : 'a list -> 'a list -> 'a list) =
  raise ImplementMe

let (fastUnion : 'a list -> 'a list -> 'a list) =
  raise ImplementMe
                
let (intersection : 'a list -> 'a list -> 'a list) =
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
