(* simplify *) 

let rec length = 
	fun (l: int list) -> 
	match l with 
		[] -> 0
	|	h::t -> 1 + (length t);; 
	
(* reduce rold, fold_right (in ocaml) *) 

(* 
Exercise:
- define fold_left
*) 

(* fold_right can define map and filter
	Exercise: define map and filter using List.fold_right *) 
	
let map f l = List.fold_right (fun hd amp_f_tl -> f had :: amp_f_tl) l []);;

(*Exercise: define filter using List.fold_right *) 

(* Data Types 
- user-defined data types give us a way to define our own abstractions: a type with associated operations
- managing complexity of programs 
- decomposing 
- preserving invaraints of programs  

What kinds of user-defined data types are supported in other languages? 
- class/object 
- C: struct, union, enum 

Ocaml has these kinds of things, but they are specialized for:
- pattern matching 
- immutability 
pe sign = Pos | Neg | Zero;;
*)

type sign = Pos | Neg | Zero;; 
(* 
defines a a new type sign
- three values taht have type "sign"

Note:
- type names must start with a lower case letter 
- Pos/Neg/Zero are called the constructors of sign 
- constructor names must start with an upper case letter 
*)

let signToInt s = 
 match s with 
  Pos -> 1
 | Neg-> -1 
 | Zero -> 0 
;;

(* a type with some fields/data, like a struct in C *) 

type point = Point of (floast * float);; 
	(* SYntax: <conttructor_name> of <type> *) 
	
let negate p = 
	match p with 
		Point(x,y) -> Point(-.x, -. y) 
;;

let negate (Point(x,y)) = Point(-.x, -.y);;

type point2 = float * float;; 
	(* point2 is a just a new name (abbreviation for) for (float * float) *) 
	(* kuje ttoedef ub C * ) *)
	
type nullableInt = Null | NonNull of int;;

(* combines the ideas of two previous exmaples:
	- two ways of constructing a nullableInt
	- we have associated data/fields 
*)

(* example: increment a nullableInt *) 

let incNullableInt x = 
	match x with 
		Null -> Null 
	|	NonNull i -> NonNull (i+1);; 
	
let incNullableInt (NonNull x) = (x +1);; 

type 'a nullable = Null | NonNull of 'a;; 

let updNullable f n = 
	match n with ull 
|	NonNull -> 

(* nullable is wiedly used in fp usually called by other names:
	- in ocaml:
	"nullable" is called option. 
	"Null" is called None
	"NonNull" is called Some 
*) 

(* defined a function that returns the nth element of a list *) 

let rec get n l =
	fun -> match l with 
	match n with 
		0 -> {match l with 
			[] -> []
		|	h::t-> h
		}
		| _ -> {match l with 
			[] -> []
			h::t-> get (n-1) t
			}
;;

let rec get n l = 
	match (n,l) with 
		(0, h::_) -> h
	|	(_,_::t) -> get (n-1) t 
	|	(_, [])	->	None
		

