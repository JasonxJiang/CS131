
exception ImplementMe

(* Problem 1: Vectors and Matrices *)

(* type aliases for vectors and matrices *)            
type vector = float list                                 
type matrix = vector list


let (vplus : vector -> vector -> vector) =
  fun v1 v2 -> List.map2 (+.) v1 v2;; 


let (mplus : matrix -> matrix -> matrix) =
  fun m1 m2 -> List.map2 vplus m1 m2

(* vmult is a helper function for dotprod *)

let  (vmult : vector -> vector -> vector) = 
  fun v1 v2 -> List.map2 ( *. ) v1 v2;;

let (dotprod : vector -> vector -> float) =
  fun v1 v2 -> let composed = vmult v1 v2 in
    List.fold_left (+.) 0. composed ;;

let generateEmptyList =
  fun m1 -> match m1 with 
      [] -> []
    | h::t -> List.map (fun x -> []) (List.hd m1)


let (transpose : matrix -> matrix) =
  fun m -> List.map (fun l -> List.rev l) (List.fold_left (fun row firstPartMatrix -> List.map2 (fun x y-> x::y) firstPartMatrix row) (generateEmptyList m) m)
 
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

(*2b Execute Function *)
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

(*)
let rec (rightTreetoList : btree -> instr list -> instr list list -> instr list * instr list list) = 
  fun tree buildingList allLists -> match tree with 
    Leaf(v) -> ([Push(v)], allLists)
   | Node(ltree, op, rtree) -> let (lbuildList, lAllLists) = (rightTreetoList ltree buildingList allLists) in 
                                let (rbuildList, rAllLists) = (rightTreetoList rtree buildingList allLists) in 
                                  let swapIncluded = (lbuildList@rbuildList@[Swap]@[Calculate(op)]) in
                                    let swapExcluded = lbuildList@rbuildList@[Calculate(op)] in 
                                      match op with 
                                        Minus -> (buildingList, allLists@swapIncluded@swapExcluded)
                                      | Divide -> (buildingList, allLists@swapIncluded@swapExcluded)
                                      | _ ->  (buildingList, allLists@swapExcluded)
*)

(* helper functions for compileOpt *)
let rec (rightExptoTree : exp -> btree) = 
  fun input -> match input with 
      Num(v) -> Leaf(v) 
    | BinOp(l,op,r) -> Node(expToTree r, op, expToTree l) 


let rec (rightTreetoList : btree -> instr list) = 
  fun tree -> match tree with 
    Leaf(v) -> [Push(v)]
   | Node(ltree, op, rtree) -> match op with 
        Minus -> ((rightTreetoList ltree)@(rightTreetoList rtree )@[Swap]@[Calculate(op)])
      | Divide -> ((rightTreetoList ltree)@(rightTreetoList rtree )@[Swap]@[Calculate(op)])
      | _ ->  ((rightTreetoList ltree)@(rightTreetoList rtree )@[Calculate(op)]) 


let (compileRight: exp -> instr list)  =
  fun input -> match input with 
      Num(v) -> [Push(v)]
  | BinOp(l, op, r) -> let treeInput = rightExptoTree input in 
                        rightTreetoList treeInput

let rec (countSize : instr list -> int -> float list -> int ) = 
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

(*)
(*Testing Cases *)

(*Matrices *)

let v1 = [1.;2.;3.];;
let v2 = [4.;5.;6.];;
let v3 = [7.;8.;9.];;
let m1 = [v1;v2;v3];;
let m2 = [v3;v2;v1];;
let m3 = [v1;v2];;
let m4 = [v1]
let transposem1 = [[1.;4.;7.];[2.;5.;8.];[3.;6.;9.]]

let oneD = [[5.]]
let twoD = [[1.;2.];[3.;4.]]
let threeD = m1
let fourD = [[1.;2.;3.;4.];[5.;6.;7.;8.];[9.;10.;11.;12.];[13.;14.;15.;16.]]

(*vplus tests *)
let () = assert ((vplus v1 v2) = [5.;7.;9.])
let () = assert (vplus [5.] [6.] = [11.])

(*mplus tests *)
let () = assert (mplus oneD oneD = [[10.]])
let () = assert (mplus twoD twoD = [[2.;4.];[6.;8.]])
let () = assert ((mplus m1 m2) = [[8.;10.;12.];[8.;10.;12.];[8.;10.;12.]])

(*dotprod tests *)
let () = assert ((dotprod [5.] [6.]) = 30.)
let () = assert (dotprod v1 v2 = 32.)
let () = assert (dotprod [7.;8.] [0.;-1.] = -8.)

(*transpose tests*)
let () = assert (transpose m1 = transposem1)
let () = assert (transpose m3 = [[1.;4.];[2.;5.];[3.;6.]])
let () = assert (transpose [[5.]] = [[5.]])
let () = assert (transpose [] = [])
let () = assert (transpose [[1.];[2.];[3.]] = [v1])
let () = assert (transpose [v1] = [[1.];[2.];[3.]] )
let () = assert (transpose fourD = [[1.;5.;9.;13.];[2.;6.;10.;14.];[3.;7.;11.;15.];[4.;8.;12.;16.]])

(*mmult tests *)
let () = assert (mmult m1 m1 = [[30.;36.;42.];[66.;81.;96.];[102.;126.;150.]])
let () = assert (mmult [[5.]] [[6.]] = [[30.]])
let () = assert (mmult m3 (transpose m3) = [[14.;32.];[32.;77.]])
let () = assert (mmult [v1] (transpose [List.rev v1]) = [[10.]])

(*Problem 2 Test Cases*)

let exp1 = BinOp(BinOp(Num 1.0, Plus, Num 2.0), Times, Num 3.0)
(* (((69 + 1)/7) -8) * 2 ) *)
let exp2 = (BinOp(BinOp(BinOp(BinOp(Num(69.), Plus, Num(1.)), Divide, Num(7.)), Minus, Num(8.)), Times, Num(2.)))
(* 5*6 *)
let exp3 = (BinOp(Num(5.), Times, Num(6.)))
(* (2/10) * (10/2) *)
let exp4 = (BinOp(BinOp(Num(2.), Divide, Num(10.)), Times, BinOp(Num(10.), Divide, Num(2.))))
let exp5 = (Num(4.))
let exp6 = BinOp(Num(1.), Minus, BinOp(BinOp(Num(3.), Times, Num(8.)),Divide, Num(6.)))
let exp7 = BinOp(Num(1.), Minus, BinOp(Num(6.), Divide, BinOp(Num(3.), Times, Num(8.))))
let exp8 = (BinOp(BinOp(Num(2.), Minus, Num(10.)), Times, BinOp(Num(10.), Minus, Num(2.))))

(*evalExp tests *)
let () = assert (evalExp exp1 = 9.)
let () = assert (evalExp exp2 = 4.)
let () = assert (evalExp exp3 = 30.)
let () = assert (evalExp exp4 = 1.)


(* (3*8)/6 - 1 *)
let insnSet1 = [Push 3.; Push 8.; Calculate Times; Push 6.; Calculate Divide; Push 1.; Calculate Minus]
(* 1- (3*8)/6  *)
let insnSet2 = [Push 3.; Push 8.; Calculate Times; Push 6.; Calculate Divide; Push 1.; Swap; Calculate Minus]
(* 1- (6/(3*8)) *)
let insnSet3 = [Push 3.; Push 8.; Calculate Times; Push 6.; Swap; Calculate Divide; Push 1.; Swap; Calculate Minus]
(* 4 *) 
let insnSet4 = [Push 4.]
(* 2 + 5 *)
let insnSet5 = [Push 2.; Push 5.; Swap; Calculate Plus]
let insnSet6 = [Push 2.; Push 5.; Calculate Plus]


(*execute tests*)
let () = assert (execute([Push 1.0; Push 2.0; Calculate Plus; Push 3.0; Calculate Times]) = 9.) 
let () = assert (execute insnSet1 = 3.)
let () = assert (execute insnSet2 = -3.)
let () = assert (execute insnSet3 = 0.75  )
let () = assert (execute insnSet4 = 4.)
let () = assert ((execute insnSet5) = (execute insnSet6))

let compileExp1 = [Push 1.; Push 2.; Calculate Plus; Push 3.; Calculate Times]
let compileExp2 = [Push 69.; Push 1.; Calculate Plus; Push 7.; Calculate Divide; Push 8.;Calculate Minus; Push 2.; Calculate Times]
let compileExp3 = [Push 5.; Push 6.; Calculate Times]
let compileExp4 = [Push 2.; Push 10.; Calculate Divide; Push 10.; Push 2.; Calculate Divide; Calculate Times]
let compileExp5 = [Push 4.]


(*compile tests*)
let () = assert (compile exp1 = compileExp1)
let () = assert (compile exp2 = compileExp2)
let () = assert (compile exp3 = compileExp3)
let () = assert (compile exp4 = compileExp4)
let () = assert (compile exp5 = compileExp5)


(*decompile tests*)
let () = assert (decompile compileExp1 = exp1)
let () = assert (decompile compileExp2 = exp2)
let () = assert (decompile compileExp3 = exp3)
let () = assert (decompile compileExp4 = exp4)
let () = assert (decompile compileExp5 = exp5)
let () = assert (decompile insnSet2 = exp6)
let () = assert (decompile insnSet3 = exp7)
*)