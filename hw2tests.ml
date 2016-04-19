(*Testing Cases *)

# use "hw2.ml";;
# use "hw2tests1.ml";;
# use "other_test.ml";;
(*Matrice s *)

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