
(* A simple test harness for the MOCaml interpreter. *)

(* put your tests here:
   each test is a pair of a MOCaml declaration and the expected
   result:
     - the MOCaml declaration is a string of exactly what you would type into the interpreter prompt,
       without the trailing ";;"
     - the expected result is a string of exactly what you expect the interpreter to print as a result
   use the string "dynamic type error" as the result if a DynamicTypeError is expected to be raised.
   use the string "match failure" as the result if a MatchFailure is expected to be raised.
   use the string "implement me" as the result if an ImplementMe exception is expected to be raised

   call the function runtests() to run these tests
*)
let tests = [
    (* YOU NEED TO ADD A LOT MORE TESTS! *)
		("3", "3"); 
		("false", "false");
		("let x = 34", "val x = 34");
		("y", "dynamic type error");
		("x + 4", "38");
    ("let z = 5 > 0", "val z = true");
    ("let rec mult x = function y -> x * y", "val mult = <fun>");
    ("mult 5 6", "30");
    ("let rec fact n = if (n=0) then 1 else n * fact(n-1)", "val fact = <fun>");
    ("fact 5", "120");
    ("let rec fact n = match n with 0 -> 1 | _ -> n* fact(n-1)" , "val fact = <fun>");
    ("fact 5", "120");
    ("let rec add (x,y) = x +y", "val add = <fun>");
    ("add (5,6)", "11");
    ("let y = x * 10", "val y = 340");      (* Evaluating Let with internal expression *) 
    ("let f = (function z -> z * x)", "val f = <fun>"); (* Creating a function *)
    ("f 2", "68");                          (* Confirming function works *)
    ("let x = 15", "val x = 15");
    ("f 2", "68");                          (* Confirming that function definition doesn't change *)
    ("(1,2,true)","(1, 2, true)");          (* Evaluating tuple creation *)
    ("Node(Leaf,1,Leaf)","Node (Leaf, 1, Leaf)"); (* Creating Data values *)
    ("-100","-100");                        (* Evaluating Negate'd IntConst *)
    (* Creating a match function and testing *)
    ("let m = function a -> function b -> match (a,b) with (true,1) -> 1 | (Leaf(a,b),_) -> a + b | (_,false) -> 2", "val m = <fun>");
    ("m true 1","1");                       (* Generic tests *)
    (*("m (Leaf(5,6)) true", "11");*)
    ("m 1 false","2");
    ("m Leaf false", "2");                  (* Confirming DataVal matching *)
    (*("m 1 2", "match failure");             (* Confirming Match Failure when no expressions match *)
     *)   (* Creating staged functions (anonymity) *)
    ("let k = function a -> function b -> a + b","val k = <fun>");
    ("let ks = k 1","val ks = <fun>");
    ("ks 4","5");                           (* Returned function must work *)
    (* Recursive function testing *)
    ("let rec facth n = function r -> if n = 1 then r else ((facth (n-1)) (n*r))", "val facth = <fun>");
    ("let factorial = function n -> facth n 1", "val factorial = <fun>");
    ("factorial 10", "3628800");            (* Confirm that recursion is created/referenced correctly *)
		
    (* Error handling *)
    ("match 1 with 0 -> 100","match failure");              (* incomplete match statement *)
    ("unbound", "dynamic type error");                      (* unbound variable should fail *)
    ("-(1,2)","dynamic type error");                        (* negate non-integer fails *)
    ("if (1+2) then true else false","dynamic type error"); (* if must have boolean condition *)
    ("(1+2) 3","dynamic type error");                       (* cannot invoke non-function *)
  
     (* Interesting cases *)
    (*("let rec thePathFewTravel x = if x=0 then thePathFewTravel 0 else 1", "val thePathFewTravel = <fun>");
    ("thePathFewTravel 1", "1");                            (* Emerson rolls infinitely in his grave. *)
    ("let phantomOfTheFunction = 100","val phantomOfTheFunction = 100");
    ("let phantomOfTheFunction = function x -> phantomOfTheFunction 1","val phantomOfTheFunction = <fun>");
    ("phantomOfTheFunction 2", "dynamic type error");       (* He lingers in the environment. *)
    *)
    ]

(* The Test Harness
   You don't need to understand the code below.
*)
  
let testOne test env =
  let decl = main token (Lexing.from_string (test^";;")) in
  let res = evalDecl decl env in
  let str = print_result res in
  match res with
      (None,v) -> (str,env)
    | (Some x,v) -> (str, Env.add_binding x v env)
      
let test tests =
  let (results, finalEnv) =
    List.fold_left
      (fun (resultStrings, env) (test,expected) ->
	let (res,newenv) =
	  try testOne test env with
	      Parsing.Parse_error -> ("parse error",env)
	    | DynamicTypeError _ -> ("dynamic type error",env)
	    | MatchFailure -> ("match failure",env)
	    | ImplementMe s -> ("implement me",env) in
	(resultStrings@[res], newenv)
      )
      ([], Env.empty_env()) tests
  in
  List.iter2
    (fun (t,er) r ->
      let out = if er=r then "ok" else "expected " ^ er ^ " but got " ^ r in
      print_endline
	(t ^ "....................." ^ out))
      tests results

(* CALL THIS FUNCTION TO RUN THE TESTS *)
let runtests() = test tests
  
