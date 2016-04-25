(* Name: Jason Jiang

   UID:

   Others With Whom I Discussed Things:

   Other Resources I Consulted:
   
*)

(* EXCEPTIONS *)

(* This is a marker for places in the code that you have to fill in.
   Your completed assignment should never raise this exception. *)
exception ImplementMe of string

(* This exception is thrown when a type error occurs during evaluation
   (e.g., attempting to invoke something that's not a function).
   You should provide a useful error message.
*)
exception DynamicTypeError of string

(* This exception is thrown when pattern matching fails during evaluation. *)  
exception MatchFailure  

(* EVALUATION *)

(* See if a value matches a given pattern.  If there is a match, return
   an environment for any name bindings in the pattern.  If there is not
   a match, raise the MatchFailure exception.
*)
let rec patMatch (pat:mopat) (value:movalue) : moenv =
  match (pat, value) with
      (* an integer pattern matches an integer only when they are the same constant;
	 no variables are declared in the pattern so the returned environment is empty *)
      (IntPat(i), IntVal(j)) when i=j -> Env.empty_env()
    | (BoolPat(i), BoolVal(j)) when i=j -> Env.empty_env()
    | (WildcardPat, _) -> Env.empty_env()
    | (VarPat(s), _)  -> Env.add_binding s value (Env.empty_env())  
    | (TuplePat(patL), TupleVal(valL)) -> let envs_list = (List.map2 patMatch patL valL) in
    	List.fold_left (fun currEnv previousEnvs -> (Env.combine_envs currEnv previousEnvs)) (Env.empty_env()) envs_list
 	| (DataPat(pats, patopt), DataVal(vals, valopt)) -> (Env.add_binding pats (DataVal(vals,valopt)) (Env.empty_env()))  (*Could be wrong *)
    | _ -> raise (ImplementMe "pattern matching not implemented")

    
(* Evaluate an expression in the given environment and return the
   associated value.  Raise a MatchFailure if pattern matching fails.
   Raise a DynamicTypeError if any other kind of error occurs (e.g.,
   trying to add a boolean to an integer) which prevents evaluation
   from continuing.
*)


let rec evalExpr (e:moexpr) (env:moenv) : movalue =
  match e with
      (* an integer constant evaluates to itself *)
    IntConst(i) -> IntVal(i)
    | BoolConst(b) -> BoolVal(b)
    | Var(s) ->  (*(Env.lookup s env)*)
    			(try (Env.lookup s env) with 
    				Env.NotBound -> raise (DynamicTypeError "dynamic type error"))
    | BinOp(e1, op, e2) -> 
    	(let e1' = evalExpr e1 env in 
    		let e2' = evalExpr e2 env in 
    			match (e1', e2') with 
    				(IntVal(i1), IntVal(i2)) -> (match op with 
    											Plus -> IntVal(i1 + i2)
    										| Minus -> (IntVal(i1-i2))
    										| Times -> (IntVal(i1*i2))
    										| Eq -> (BoolVal(i1=i2)) 
    										| Gt -> (BoolVal(i1>i2)))
    				| (_,_) -> raise (DynamicTypeError "can only perform BinOp operations on Ints!")) 
    | Negate(e0) -> 
    	(let v0 = evalExpr e0 env in 
    	(match v0 with 
    		IntVal i -> IntVal(-i)
    	| _ -> raise (DynamicTypeError "can only negate integers")))
    | If(c,if_clause,then_clause) ->
    	 (let v0 = evalExpr c env in 
    	 (match v0 with 
    	 	BoolVal(b) -> if b then (evalExpr if_clause env) else (evalExpr then_clause env)
    	 | _ -> raise(DynamicTypeError "cannot evaulate a non boolean value!")))
    | Function(pat, e) ->  FunctionVal(None, pat, e, env)
    | Tuple(l) -> TupleVal(List.map(fun elem -> evalExpr elem env) l)  
    | Data(s,expr_op) -> (match expr_op with 
    						None -> DataVal(s, None)
    					| Some(something) -> let value = evalExpr something env in 
    						DataVal(s,Some(value)))
	| Match(e, l) -> 
    	let evalE = evalExpr e env in 
    		IntVal(69)
    (*| FunctionCall(e1, e2) -> 
    			(let funcVal = evalExpr e1 env in 
    					match evalExpr e1 env with 
    						FunctionVal(func_s,func_pat,func_expr,func_env) -> 
    							(let args = evalExpr e2 in 
    								match args with 
    									) *)
(*Match e2 with different types of moexpr and then bind them in an envrionment correclty
Or match with movalue*)

    		(*let rec aux pat val -> patMatch *)(* goal is if evalE matches a pattern in the list then evalExpr the accompanying moexpr *)


(* Evaluate a declaration in the given environment.  Evaluation
   returns the name of the variable declared (if any) by the
   declaration along with the value of the declaration's expression.
*)
let rec evalDecl (d:modecl) (env:moenv) : moresult =
  match d with
      (* a top-level expression has no name and is evaluated to a value *)
      Expr(e) -> (None, evalExpr e env)
    | Let(s, let_expr) -> let binded_val = (evalExpr let_expr env) in
                     		    (Some(s), binded_val)
    | LetRec(s, rec_expr) -> let binded_val = (evalExpr rec_expr env) in 
    							match binded_val with 
    								FunctionVal(bind_s, pat, bind_e, bind_env)  -> let bind_moval = (FunctionVal(Some(s), pat, bind_e, bind_env)) in
    								(Some(s), bind_moval)
    							    				

