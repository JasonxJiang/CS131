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


(*Midterm Exam

Closed everything 
M/C 
Short Answer 
Coding 

No Tail Recursion 
OCaml
-Recursion 
-pattern matching 
-higher-order functions 
 -map, filter, fold_right/left
-exceptions
-datatypes 

Concepts:
 - static vs. dynamic scoping 
 - static vs dynamic typechecking  
 - strong vs. weak typechecking 
 - parametric polymoprhism vs. static overloading 

Parametric polymorphism:

the type of a funciton includes one or more *type variables*S
- caller gets to choose what types to instantiate the type variables with 
- makes typecheking more expressive 
 - one piece of code that can be passed arguments of all possible types 
 - more expressions can safely pass type checking 

Static overloading: 

many difffertn functions that all have the same name 
+ for ints, + for floats, + for strings 

3+4 (compiler figures out you want 3+4)

 let id = function x -> x
 (id 3) : int 
 (id false) : bool

 Scoping:

 static (or lexical) scoping:
  - you can determine the variable declaration associated with each variable use at compile time
  - every variable use is referring to the nearest lexically enclosing delcaration of that variable

 let x = 3 in 
 let y = 4 in 
 	y + (let x = 5 in x) + x 

 	Note the first (let x = 5 in x) is bound to five wherease the second x outside the 
 	(let x = 5 in x) is bound to 3 

 	(let x = 3 in function y -> y+x) 
    let x = 0 in
 	in f 2

 	Note this will still return 5 although x has changed to 0 
 	A variable can outlive its scope 

Strong vs weak typechecking 

strongly typed means that a program never gets into an undefined state
 - either rejected at compile time 
 - or get a run-time excpetion before something bad happens 

weakly typed means that a program can get into an undefined state (benefit is performance)

key issue: memory safety  ruling out memory errors 
 - most languages prevent memory errors 

*)
