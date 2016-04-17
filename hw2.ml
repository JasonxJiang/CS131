
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
let m4 = [v1]
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
    | h::t -> List.map (fun x -> []) (List.hd m1)


let (transpose : matrix -> matrix) =
  fun m -> List.map (fun l -> List.rev l) (List.fold_left (fun row firstPartMatrix -> List.map2 (fun x y-> x::y) firstPartMatrix row) (generateEmptyList m) m);;


 
let (mmult : matrix -> matrix -> matrix) =
  fun m1 m2 -> let m22 = transpose m2 in 
    List.map (fun m1row -> (List.map(fun m2row -> dotprod m1row m2row) m22)) m1

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


                                                                      
(*helper function for execute*)
let rec executeHelper1 (*(executeHelper1 : instr list -> float list -> float)*)= 
  fun insnLst stack -> match insnLst with 
      [] -> List.hd stack 
    | h::t -> match h with 
           Push(v) -> executeHelper1 t (stack@[v])
        |  Calculate(operator) -> if (List.length stack >1) then 
                                  let rOp = last_Element stack in 
                                    let lOp = second_Last_Elem stack in 
                                      let simpleResult = simpleOp (BinOp(Num(lOp), operator, Num(rOp))) in
                                        let stackOpsGone = remove_last_two stack in  
                                          executeHelper1 t (stackOpsGone@[simpleResult])
                                  else 
                                    -1.
        |  Swap -> if (List.length stack > 1) then 
                    let rOp = last_Element stack in 
                      let lOp = second_Last_Elem stack in 
                        let addList = ([rOp]@[lOp]) in 
                          executeHelper1 t ((remove_last_two stack)@addList)
                    else 
                      -1.

(*2a Execute Function *)
let (execute : instr list -> float) =
  fun l -> match l with 
      [] -> 0.
    | h::t -> executeHelper1 l []      





(* A type for converting EXP into Binary Trees *)
type btree = Leaf of float | Node of btree * op * btree 

(*helper functions for compile *)
let rec (expToTree : exp -> btree) =
  fun input -> match input with 
      Num(v) -> Leaf(v)
    | BinOp(l,op,r) -> Node(expToTree l, op, expToTree r) 

let rec (treeToList : btree -> instr list) =
    fun tree -> match tree with 
        Leaf(v) -> [Push(v)]
      | Node(ltree, op, rtree) -> ((treeToList ltree)@(treeToList rtree )@[Calculate(op)])

(*2c. Compile Function *)
let rec (compile : exp -> instr list) =
  fun input -> match input with 
      Num(v) -> [Push(v)]
    | BinOp(l, op, r) -> let treeInput = expToTree input in
                          treeToList treeInput





(*helper function for decompile *)
let rec (decompileHelper : instr list -> exp list -> exp) =
  fun insnLst stack -> match insnLst with 
    [] -> List.hd stack 
  | h::t -> match h with 
      Push(v) -> decompileHelper t (stack@[Num(v)])
    | Calculate(op) -> let rOp = last_Element stack in 
                          let lOp = second_Last_Elem stack in 
                            decompileHelper t ((remove_last_two stack)@[BinOp(lOp,op,rOp)])
    | Swap -> if (List.length stack >1) then 
                  let rOp = last_Element stack in 
                    let lOp = second_Last_Elem stack in 
                      decompileHelper t ((remove_last_two stack)@[rOp]@[lOp])
              else 
                Num(-1.)

(*2d. Decompile Function *)                         
let (decompile : instr list -> exp) =
  fun lst -> match lst with 
      [] -> Num(-1.)
    | h::t -> decompileHelper lst [] 




(* helper functions for compileOpt *)
let rec rightExptoTree = 
  fun input -> match input with 
      Num(v) -> Leaf(v) 
    | BinOp(l,op,r) -> Node(expToTree r, op, expToTree l) 

let rec rightTreetoList = 
  fun tree -> match tree with 
    Leaf(v) -> [Push(v)]
   | Node(ltree, op, rtree) -> match op with 
        Minus -> ((treeToList ltree)@(treeToList rtree )@[Swap]@[Calculate(op)])
      | Divide -> ((treeToList ltree)@(treeToList rtree )@[Swap]@[Calculate(op)])
      | _ ->  ((treeToList ltree)@(treeToList rtree )@[Calculate(op)])  


let compileRight =
  fun input -> match input with 
      Num(v) -> [Push(v)]
  | BinOp(l, op, r) -> let treeInput = rightExptoTree input in 
                        rightTreetoList treeInput

let rec countSize (*(countSize: instr list -> int -> list float -> int)*) = 
  fun lst maxSize stack -> match lst with 
      [] -> maxSize
    | h::t -> match h with 
        Push(v) -> let addPush = (stack@[v]) in
                    let currSize = List.length addPush in
                      if (currSize > maxSize) then 
                        countSize t (currSize) addPush 
                      else countSize t maxSize addPush 
      | Calculate(op) -> if (List.length stack >1) then 
                          let rOp = last_Element stack in 
                            let lOp = second_Last_Elem stack in 
                              let simpleResult = simpleOp (BinOp(Num(lOp),op,Num(rOp))) in
                                let newStack = (remove_last_two stack)@[simpleResult] in
                                  if (List.length newStack > maxSize) then 
                                   countSize t (List.length newStack) newStack 
                                  else 
                                    countSize t maxSize newStack
                          else 
                            -1
      | Swap -> if (List.length stack >1) then 
                  let rOp = last_Element stack in 
                    let lOp = second_Last_Elem stack in 
                      countSize t maxSize ((remove_last_two stack)@[rOp]@[lOp])
                else 
                  -1





(* EXTRA CREDIT *)     
(*2e. CompileOpt Function *)
 
let (compileOpt : exp -> (instr list * int)) =
  fun input -> let leftCompile = compile input in
                let maxSizeL = countSize leftCompile 0 [] in 
                  let rightCompile = compileRight input in 
                    let maxSizeR = countSize rightCompile 0 [] in 
                      if (maxSizeL < maxSizeR) then 
                        (leftCompile, maxSizeL)
                      else 
                        (rightCompile, maxSizeR)



let test1 = [Push 1.;Push 2.; Calculate Plus]
let test2 = [Push 1.; Push 2.; Calculate Plus; Push 3.; Calculate Times]    
let rtest = BinOp(Num(1.), Minus,BinOp(Num(2.), Plus, Num(3.)))