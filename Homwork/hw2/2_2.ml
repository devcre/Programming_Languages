type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list
              
let rec diff(ae, str) =
  
  let rec hasStr list str =
    match list with
    | [] -> false
    | hd::tl -> 
        match hd with
        | VAR x -> if x = str then true || (hasStr tl str)
            else false || (hasStr tl str)
        | POWER(x, _) -> if x = str then true || (hasStr tl str)
            else false || (hasStr tl str)
        | _ -> (hasStr tl str)
  in

  (* TIMES *)
  
  let rec hasDupl list str =
    match list with
    | [] -> 0
    | hd::tl -> if hd = str then 1 + (hasDupl tl str)
        else hasDupl tl str
  in
  
  (* 2 *)

  let diff_tim_one(ae, str, bool) =
    match ae with 
    | CONST v -> [CONST v]
    | VAR x -> if x = str then [CONST 1]
        else [VAR x]
    | POWER(x, v) -> if (x = str) && (v > 2) then [CONST v; POWER(x, v-1)]
        else if (x = str) && (v = 2) then [CONST 2; VAR x]
        else if (x = str) && (v = 1) then [CONST 1]
        else if bool = true then [POWER(x, v)]
        else [CONST 0]
    | TIMES alist -> [diff(TIMES alist, str)]
    | SUM alist -> [diff(SUM alist ,str)]
  in
  
  (* 2 *)

  let rec diff_tim_true(list, str) = 
    match list with
    | [] -> []
    | hd::tl -> diff_tim_one(hd, str, true)@diff_tim_true(tl, str)
  in 
  
  (* remove duplicated variables(str) and join to POWER *)
  let rec makePow list newlist str count =
    match list with
    | [] -> POWER(str, count)::newlist
    | hd::tl -> 
        if hd = (VAR str) then makePow tl tl str (count+1)
        else makePow tl newlist str count
  in
  
  (* 1 *)
  let diff_tim(list, str) =
    if hasStr list str = true then
      if (hasDupl list (VAR str)) > 1 then
        match makePow list [] str 0 with
        | [] -> []
        | hd::tl -> diff_tim_one(hd, str, true)@diff_tim_true(tl, str)
      else
        match list with
        | [] -> []
        | hd::tl -> diff_tim_one(hd, str, true)@diff_tim_true(tl, str)
    else [CONST 0]
  in 
  
  (* if TIMES ae list's length is 1 then convert to one ae type *)
  let conv_tim ae =
    match ae with
    | TIMES alist -> if List.length alist = 1 then List.hd alist
        else TIMES alist
    | _ -> ae
  in
  
  (* SUM *)
  (* 2 *)
  let rec diff_sum_one(ae, str) =
    match ae with
    | CONST v -> []
    | VAR x -> if x = str then [CONST 1]
        else []
    | POWER(x, v) -> if (x = str) && (v > 2) then [TIMES [CONST v; POWER(x, v-1)]]
        else if (x = str) && (v = 2) then [TIMES [CONST 2; VAR x]]
        else if (x = str) && (v = 1) then [CONST 1]
        else [CONST 0]
    | TIMES alist -> [(TIMES (diff_tim(alist, str)))]
    | SUM alist -> [diff(SUM alist, str)]
  in
  
  (* 1 *)
  let rec diff_sum(list, str) =
    match list with
    | [] -> []
    | hd::tl -> diff_sum_one(hd, str)@diff_sum(tl, str)
  in 
  
  (********************** MAIN **********************)

  match ae with
  | CONST v -> CONST 0
  | VAR x -> if x = str then CONST 1
      else CONST 0
  | POWER(x, v) -> if (x = str) && (v > 2) then TIMES [CONST v; POWER(x, v-1)]
      else if (x = str) && (v = 2) then TIMES [CONST 2; VAR x]
      else if (x = str) && (v = 1) then CONST 1
      else CONST 0
  | TIMES alist -> conv_tim (TIMES (diff_tim(alist, str)))
  | SUM alist -> SUM (diff_sum(alist, str))