let y = Env.add_binding "x" 34 (Env.empty_env())
let z = Env.add_binding "hi" 65 (Env.empty_env())
let envs_list = [y;z];;
let x = List.fold_left (fun currEnv previousEnvs -> (Env.combine_envs currEnv previousEnvs)) (Env.empty_env()) envs_list

let check x = 
	match x with 
		Some(s) -> s
	| None -> "hello" 