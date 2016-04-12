
exception ImplementMe

(* Problem 1: Vectors and Matrices *)

(* type aliases for vectors and matrices *)            
type vector = float list                                 
type matrix = vector list

let (vplus : vector -> vector -> vector) =
  raise ImplementMe
(*)
let (mplus : matrix -> matrix -> matrix) =
  raise ImplementMe

let (dotprod : vector -> vector -> float) =
  raise ImplementMe

let (transpose : matrix -> matrix) =
  raise ImplementMe

let (mmult : matrix -> matrix -> matrix) =
  raise ImplementMe

        
(* Problem 2: Calculators *)           
           
(* a type for arithmetic expressions *)
type op = Plus | Minus | Times | Divide
type exp = Num of float | BinOp of exp * op * exp

let (evalExp : exp -> float) =
  raise ImplementMe

(* a type for stack instructions *)	  
type instr = Push of float | Swap | Calculate of op

let (execute : instr list -> float) =
  raise ImplementMe
      
let (compile : exp -> instr list) =
  raise ImplementMe

let (decompile : instr list -> exp) =
  raise ImplementMe

(* EXTRA CREDIT *)        
let (compileOpt : exp -> (instr list * int)) =
  raise ImplementMe
*)

