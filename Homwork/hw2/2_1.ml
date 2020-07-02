type exp = X | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp
  
let calculate exp = 
  let env = [] in
  
  let extend_env value env = value::env in
  
  let findVal exp env =
    match env with
    | [] -> raise (Failure ("Error"))
    | h::t -> h
  in
  
  let rec dist_cal exp env =
    match exp with
    | X -> findVal exp env
    | INT i -> Int.to_float i
    | REAL i -> i
    | ADD(a, b) -> (dist_cal a env) +. (dist_cal b env)
    | SUB(a, b) -> (dist_cal a env) -. (dist_cal b env)
    | MUL(a, b) -> (dist_cal a env) *. (dist_cal b env)
    | DIV(a, b) -> (dist_cal a env) /. (dist_cal b env)
    | SIGMA(x1, x2, ex) ->
        if dist_cal x1 env > dist_cal x2 env then 0.0
        else
          let a = dist_cal x1 env in
          let e = SIGMA(ADD(x1, INT 1), x2, ex) in
          (dist_cal ex (extend_env a env)) +. (dist_cal e env)
    | INTEGRAL(x1, x2, ex) ->
        let expre = ADD(x1, REAL 0.1) in
        if dist_cal expre env > dist_cal x2 env then 0.0
        else
          let a = dist_cal x1 env in
          let e1 = INTEGRAL(ADD(x1, REAL 0.1), x2, ex) in
          let e2 = MUL(ex, REAL 0.1) in
          (dist_cal e2 (extend_env a env)) +. (dist_cal e1 [])
  in 
  
  dist_cal exp env