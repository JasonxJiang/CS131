(* Name: Jason Jiang

   UID: 604409327 

  Others With Whom I Discussed Things:
  Ryan Peterman (Shared testcases)
  Alex Fong (Shared testcases)
  Max Chern (Shared testcases)
  Prashanth Swami (Shared testcases and discussed implementation of assignment)
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
    | (TuplePat(patL), TupleVal(valL)) when (List.length patL) = (List.length valL)-> let envs_list = (List.map2 patMatch patL valL) in
      List.fold_left (fun currEnv previousEnvs -> (Env.combine_envs previousEnvs currEnv)) (Env.empty_env()) envs_list
    | (DataPat(pats, patopt), DataVal(vals, valopt)) -> 
      (let env_data_name_bound = 
      (Env.add_binding pats (DataVal(vals,valopt)) (Env.empty_env())) in 
      match patopt with 
      None -> env_data_name_bound
      | Some(inner_pat_val) -> (match valopt with 
                Some(inner_val_val) -> Env.combine_envs env_data_name_bound (patMatch inner_pat_val inner_val_val) (*Env.combine_envs env_data_name_bound (patMatch inner_pat_val inner_val_val) *)
                | _ -> env_data_name_bound) 
    )


    
(* Evaluate an expression in the given environment and return the
   associated value.  Raise a MatchFailure if pattern matching fails.
   Raise a DynamicTypeError if any other kind of error occurs (e.g.,
   trying to add a boolean to an integer) which prevents evaluation
   from continuing.
*)

let rec helper (pat:mopat) (value:movalue) : bool = 
  match (pat,value) with 
    (IntPat(i), IntVal(j)) when i =j -> true
    | (BoolPat(i), BoolVal(j)) when i=j -> true
    | (WildcardPat, _) -> true 
    | (VarPat(s), _)  -> true
    | (TuplePat(patL), TupleVal(valL)) when (List.length patL = List.length valL) -> let bool_list = (List.map2 helper patL valL) in 
                                          List.fold_left (fun elem acc -> elem&&acc) true bool_list
    | (DataPat(pats, patopt), DataVal(vals, valopt)) when pats=vals->( match (patopt,valopt) with
                                                      (None,None) -> true 
                                                      | (Some(s),Some(s')) -> helper s s'
                                                      | (_,_) -> false )  
    | _ -> false 

let rec matchHelper (matchVal:movalue) (patList: (mopat*moexpr) list) : (mopat *moexpr) =
  match patList with 
    [] -> raise MatchFailure
    | (pat, expr)::t -> match matchVal with 
      IntVal(i) -> (match pat with 
                  IntPat(i') ->if (i = i') then (pat,expr) else matchHelper matchVal t
                  | WildcardPat -> (pat,expr)
                  | VarPat(s) -> (pat, expr) 
                  | _ -> matchHelper matchVal t)
    | BoolVal(b) -> (match pat with 
                  BoolPat(b') -> if (b=b') then (pat,expr) else matchHelper matchVal t
                  | WildcardPat -> (pat,expr)
                  | VarPat(s) -> (pat, expr) 
                  | _ -> matchHelper matchVal t) 
    | TupleVal(l) -> (match pat with 
                    TuplePat(l') when (List.length l) = (List.length l') -> let bool_list = (List.map2 helper l' l) in 
                                      let perfectMatch = List.fold_left (fun elem acc -> elem&&acc) true bool_list in 
                                          if perfectMatch then (pat,expr) else matchHelper matchVal t
                  | WildcardPat -> (pat,expr)
                  | VarPat(s) -> (pat, expr) 
                  | _ -> matchHelper matchVal t)
    | DataVal(s, valOp) -> (match pat with 
                            DataPat(s', valOp') -> (if (s=s') then
                              match(valOp, valOp') with 
                                (None,None) -> (pat,expr)
                              | (Some(vals), Some(pats)) -> if (helper pats vals) then (pat,expr) else matchHelper matchVal t
                              | (_,_) -> matchHelper matchVal t 
                              else matchHelper matchVal t)
                            | WildcardPat -> (pat,expr) 
                            | VarPat(s) -> (pat, expr) 
                            | _ -> matchHelper matchVal t)
    | _ -> match pat with 
            VarPat(s)-> (pat,expr)
            | _ -> raise MatchFailure  



let rec evalExpr (e:moexpr) (env:moenv) : movalue =
  match e with
      (* an integer constant evaluates to itself *)
    IntConst(i) -> IntVal(i)
    | BoolConst(b) -> BoolVal(b)
    | Var(s) ->  (*(Env.lookup s env)*)
          (try (Env.lookup s env) with 
            Env.NotBound -> raise (DynamicTypeError ("dynamic type error")))
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
            | (_,_) -> raise (DynamicTypeError "dynamic type error")) 
    | Negate(e0) -> 
      (let v0 = evalExpr e0 env in 
      (match v0 with 
        IntVal i -> IntVal(-i)
      | _ -> raise (DynamicTypeError "dynamic type error")))
    | If(c,if_clause,then_clause) ->
       (let v0 = evalExpr c env in 
       (match v0 with 
        BoolVal(b) -> if b then (evalExpr if_clause env) else (evalExpr then_clause env)
       | _ -> raise(DynamicTypeError "dynamic type error 6")))
    | Function(pat, e) ->  FunctionVal(None, pat, e, env)
    | Tuple(l) -> TupleVal(List.map(fun elem -> evalExpr elem env) l)  
    | Data(s,expr_op) -> (match expr_op with 
                None -> DataVal(s, None)
              | Some(something) -> let value = evalExpr something env in 
                DataVal(s,Some(value)))
    | Match(e1, l) -> 
      let evalE = (evalExpr e1 env) in 
        let (res_pat, res_expr) = (matchHelper evalE l) in
          let new_env = (Env.combine_envs env (patMatch res_pat evalE)) in
            evalExpr res_expr new_env 
    | FunctionCall(e1, e2) -> 
          (let funcVal = evalExpr e1 env in 
              match funcVal with 
              FunctionVal(func_s,func_pat,func_expr,func_env) -> 
                  (let args = evalExpr e2 env in 
                    let new_fun_env = Env.combine_envs func_env (patMatch func_pat args) in 
                        match func_s with 
                          None -> evalExpr func_expr new_fun_env
                          | Some(rec_fun_name) -> let new_fun_env' = (Env.add_binding rec_fun_name (evalExpr e1 env) new_fun_env) in
                            evalExpr func_expr new_fun_env'
                  )
              | _ -> raise(DynamicTypeError "dynamic type error")
            )
    | _ -> raise (DynamicTypeError "dynamic type error")
        


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
                    FunctionVal(bind_s, pat, bind_e, bind_env) -> let bind_moval = (FunctionVal(Some(s), pat, bind_e, bind_env)) in
                    (Some(s), bind_moval)
                              

