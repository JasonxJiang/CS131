(* Higher-order functions *) 
(* a function that takes another function as an argument *)
(* map, filter, fold *)
(* Incorrect usage *) 

(* Correct Usage *)
List.fold_right(fun s oldCount -> oldCount + String.length s) ["hello"; "there"] 0;;
(* needs to take an accumulator as an input as well *) 

(* Implement reverse list using fold_right *)
let reverse l = 
		List.fold_right 
			(fun elem reversedRest -> reversedRest@[elem]) l [];;

let contains x l = 
	List.fold_right 
		(fun elem isInRestOfList -> (elem=x) || isInRestOfList) l false;;

(* General setup for list.fold_right 
	List.fold_right 
		(fun elem a2 -> ...) l baseCase *)

let rec twoToN n = 
	match n with 
		1 -> []
	| _ -> (twoToN (n-1)) @ [n];; 	

(* checks if n is prime *)
let isPrime n =
		let l = twoToN (n-1) in 
		List.fold_right (fun elem nothingDividesN -> (n mod elem)!=0 && nothingDividesN) l true;;


(* return a list of elements of l that are prime *) 
let primes l = 
		List.filter isPrime l;; 

(*Datatypes *)

(* Tagged union:
	-union beacuase it's an OR of several cases 
	-tagged because each case has label/tag *)
type optInt = Null | Nonnull of int

let addOptInt (oi1: optInt) (oi2: optInt) = 
	match (oi1, oi2) with 
		(Nonnull i1, Nonnull i2) -> Nonnull (i1 +  i2)
	| _ -> Null

let fOptInt f oi1 oi2 = 
	match (oi1, oi2) with 
		(Nonnull i1, Nonnull i2) -> Nonnull(f oi1 oi2)
	| _ -> Null  

type 'a option = None | Some of 'a

let rec find x l =
	let rec aux index l = 
		match l with 
			[] -> None
		| h::t -> if (x=h) then Some index else aux (index +1) t
	in aux 0 l 

(* recursive datatypes *) 

(* a list *) 
type 'a mylist = Empty | Node of 'a * 'a mylist 

let rec sumList = 
	function l ->
		match l with 
			Empty -> 0 
		| Node(h,t) -> h + sumList t;;  

(* binary trees *)
type btree = Leaf | Node of btree * btree * int

type btree2 = Leaf2 of int | TNode of btree2 * btree2

let rec treeSize t = 
	match t with 
		Leaf2 -> 0 
	| TNode(left, right, d) -> 1 + treeSize left + treeSize right  





	

