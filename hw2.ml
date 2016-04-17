
exception ImplementMe

(* Problem 1: Vectors and Matrices *)

(* type aliases for vectors and matrices *)            
type vector = float list                                 
type matrix = vector list


let (vplus : vector -> vector -> vector) =
  fun v1 v2 -> List.map2 (+.) v1 v2;; 



let v1 = [1.;2.;3.];;
let v2 = [4.;5.;6.];;
let v3 = [7.;8.;9.];;
let m1 = [v1;v2;v3];;
let m2 = [v3;v2;v1];;
let m3 = [v1;v2];;

(*Convert rows to columns *)



let (mplus : matrix -> matrix -> matrix) =
  fun m1 m2 -> List.map2 vplus m1 m2



(* vmult is a helper function for dotprod *)

let  (vmult : vector -> vector -> vector) = 
  fun v1 v2 -> List.map2 ( *. ) v1 v2;;

let (dotprod : vector -> vector -> float) =
  fun v1 v2 -> let composed = vmult v1 v2 in
    List.fold_left (+.) 0. composed ;;

let row1 = List.map (fun l -> List.nth l 0) m1;;
(* Access Column of a matrix and create a row *)

let generateEmptyList =
  fun m1 -> match m1 with 
      [] -> []
    | h -> List.map (fun x-> []) h
    | h::t -> List.map (fun x -> []) h


let (transpose : matrix -> matrix) =
  fun m -> List.map (fun l -> List.rev l) (List.fold_left (fun row firstPartMatrix -> List.map2 (fun x y-> x::y) firstPartMatrix row) (generateEmptyList m) m);;

  (* save this for later working but reversed *)
(*List.fold_left (fun row firstPartMatrix -> List.map2 (fun x y-> x::y) firstPartMatrix row) [[];[];[]] m;;
*)

(* Implement Transpose First! *)
 (*)
let (mmult : matrix -> matrix -> matrix) =
  fun m1 m2 ->  List.map2 (fun m1 m2-> List.map (fun row m2 -> (List.map (fun row2 -> dotprod row1 row2) m2 )) m1)  m1 transpose m2;;
*)
(* Multiply row by row where m1 * tranpose m2 to build row by row of resultant vector *)
     
(* Problem 2: Calculators *)           
 
(* a type for arithmetic expressions *)
type op = Plus | Minus | Times | Divide
type exp = Num of float | BinOp of exp * op * exp

let (simpleOp : exp->float) = 
  fun simple -> match simple with 
    Num(value) -> value
  | BinOp(Num(value1), operator, Num(value2)) ->  match operator with 
      Plus -> value1 +. value2
    | Minus ->  value1 -. value2 
    | Times ->  value1 *. value2 
    | Divide -> value1 /. value2 

let test = BinOp(BinOp(Num 1.0, Plus, Num 2.0), Times, Num 3.0);;

let rec (evalExp : exp -> float) =
  fun eval -> match eval with 
      Num(value) -> value 
    | BinOp(l,op,r) -> match (l,r) with 
      (BinOp(l1, op1, r1), BinOp(l2,op2,r2)) -> evalExp (BinOp(Num(evalExp l), op, Num(evalExp r) ))
    | (Num(v1), Num(v2)) -> simpleOp eval
    | (Num(v1), BinOp(l2,op2,r2)) -> evalExp (BinOp(l, op, Num(evalExp r)))
    | (BinOp(l1, op1, r1), Num(v2)) -> evalExp (BinOp(Num(evalExp l), op, r))



(* a type for stack instructions *)	  
type instr = Push of float | Swap | Calculate of op


let remove_last lst = 
    List.rev (List.tl (List.rev lst))
let remove_last_two lst = 
    remove_last (remove_last lst) 
let last_Element l =
    List.nth l (List.length l -1)
let second_Last_Elem l = 
    List.nth l (List.length l -2)



(* for executeHelper assume the insn passed along is no longer in list! *)
let rec (executeHelper: instr list -> instr -> float) = 
    fun lst insnType -> match insnType with
      Swap -> let last = last_Element lst in 
                  let secLast = second_Last_Elem lst in 
                    let twoElementsGone = remove_last(remove_last lst) in 
                        executeHelper (twoElementsGone@[last]) secLast  
    | Calculate(operator) -> let rOp = last_Element lst in 
                                let lOp = second_Last_Elem lst in 
                                  match (rOp, lOp) with 
                                  (Push(v1), Push(v2))  -> 5. (*executeHelper Push(simpleOp (BinOp(Num(v1), operator, Num(v2))))*)
                                | _ -> 5.   
(*
let rec (execute : instr list -> float) = 
  fun l -> match l with
    [] -> 0.
  | h::t -> let last = List.tl l in 
             executeHelper (remove_last l) last 
*)



let (execute : instr list -> float) =
  fun l -> if (List.length l = 1) then 
            match (List.hd l) with 
              Push(v1) -> v1 
            | _ -> 5.
          else  
              match l with 
                [] -> 0.       
              | h::t -> let lastInsn = List.nth l (List.length l -1) in  
                executeHelper (remove_last l) lastInsn



let test1 = [Push 1.;Push 2.; Swap];;

(*      
let (compile : exp -> instr list) =
  raise ImplementMe

let (decompile : instr list -> exp) =
  raise ImplementMe

(* EXTRA CREDIT *)        
let (compileOpt : exp -> (instr list * int)) =
  raise ImplementMe
*)

