
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



(*let rec (powerset : 'a list -> 'a list list) =
	fun l -> match l with 
		[] -> [[]]
	| h::t -> let rec applyToAll = 
	fun f acc l -> match l with 
		[] -> acc 
	|	h::t -> applyToAll f (f acc h) t in 
	applyToAll(fun xs t -> (h::t)::t::xs) [] (powerset t);;	*)


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

let () = assert (member 1 [1;2;3;4])
let () = assert (not (member 5 [1;2;3;4]))
let () = assert ((add 1 [1;2;3;4])=[1;2;3;4])
let () = assert ((add 1 [2;3;4])=[1;2;3;4])
(* let () = assert ((union [1;2] [3;4])=[2;1;3;4]) *)
(* let () = assert ((union [1;2] [2;3])=[1;2;3]) *)
let () = assert ((fastUnion [1;2;3;4] [2;3;4;5])=[1;2;3;4;5])
let () = assert ((fastUnion [1;2;3;4] [5;6;7;8])=[1;2;3;4;5;6;7;8])
let () = assert ((intersection [1;2;3] [2;3;4])=[2;3])
let () = assert ((setify [1;1;1;2;2;2;3;3;3])=[1;2;3])
let () = assert ((partition (fun x -> x > 2) [1;2;3;4])=([3;4], [1;2]))
let () = assert ((whle (function x -> x < 10) (function x -> x * 2) 1)=16)
let () = assert (((pow 2 (fun x -> x*x)) 3)=81)

(* testing member*)
let set1 = [1;2;3;4;5;6];;
assert((member 1 set1)=true);;
assert((member 69 set1)=false);;
assert((member 5 [])=false);;  

(* Testing add *)
let set3 = [1;2;3;4];;
assert((add 5 set3) = [5;1;2;3;4]);;
let set4 = (add 5 set3);;
assert((add 3 set4) = [5;1;2;3;4]);;

(* Testing Union *)
let set5= [1;2;3;4;5];;
let set6= [6;7;8];;
assert(union set5 set6 = [1;2;3;4;5;6;7;8]);;
assert(union set5 [] = [1;2;3;4;5]);;
assert(union [] set5 = [1;2;3;4;5]);;
let set7 = [1;2;3;4;5];;
assert(union set5 set7 = set5);;
let set8 = [1;10;3;5;7;8;13];;

(* Testing fastUnion *)
let set5= [1;2;3;4;5];;
let set6= [6;7;8];;
assert(fastUnion set5 set6 = [1;2;3;4;5;6;7;8]);;
assert(fastUnion set5 [] = [1;2;3;4;5]);;
assert(fastUnion [] set5 = [1;2;3;4;5]);;
let set7 = [1;2;3;4;5];;
assert(fastUnion set5 set7 = set5);;
let set8 = [1;3;5;7;8;10;13];;
assert(fastUnion set7 set8 = [1;2;3;4;5;7;8;10;13]);;
assert(fastUnion [] [] = []);; 

(*Testing intersection *)                
assert(intersection [1;3;5;7] [2;4;6;8] = []);;
assert(intersection [1;2;3] [1;2;3] = [1;2;3]);;
let set11 = [10;9;8;7;6];;
let set12 = [6;8;10];;

(* Testing setify *)
assert(setify [1;2;3;4;4;4;4;4;5;5;5;5;6;6;6;7;7;7;8;8;8] = [1;2;3;4;5;6;7;8]);;
assert(setify [] = []);
assert(setify [1;1] = [1]);;

(* Test partition *)
assert((partition (fun x -> x <10) [1;11;2;22;3;33;44;55;66;4;5;6]) = ([1;2;3;4;5;6],[11;22;33;44;55;66]));;
assert((partition (fun x -> x< 10) []) = ([],[]));;
assert (partition (function x -> x > 3) [1;5;4;3;2;6] = ([5;4;6], [1;3;2]));;

(*test whle *)
assert( (whle (x < 10) (fun x -> x*2) 1) = 16);;
assert( (whle (x < 10) (fun x -> x+1) 1) =10);;

(* Testing  pow *)
let g = pow 3 (fun x -> x *2);;
assert((pow 0 (fun x -> x*2)) 1 = 1);;
assert((pow 1 (fun x -> x*2)) 1 =2 );;
assert(g 2 =16);;

		







