let likes rel p =
  
  let tuple_one tu =
    match tu with
    | (e, _) -> e 
  in
  
  let tuple_two tu =
    match tu with
    | (_, e) -> e 
  in
  
  (* 3 *)
  let rec find_ele g tu = 
    match g with
    | [] -> []
    | hd::tl -> if (tuple_one hd = tuple_two tu)
        then (tuple_two hd)::(find_ele tl tu)
        else find_ele tl tu
  in
  
  (* 4 *)
  let rec adding g l tu =
    match l with
    | [] -> g
    | h::t -> if List.mem (tuple_one tu, h) g = true
        then adding g t tu
        else adding ((tuple_one tu, h)::g) t tu
  in
  
  (* 2 *)
  let add_new_tuple g tu =
    let l = find_ele g tu in
    adding g l tu
  in
  
  (* 1 *)
  let rec transitive g newg =
    match g with
    | [] -> newg
    | hd::tl -> transitive tl (add_new_tuple newg hd)
  in
  
  let rec more_tran g =
    let g2 = transitive g g in
    if (List.length g2) > (List.length g) then more_tran g2
    else g 
  in

  let rec calcul g p =
    match g with
    | [] -> 0
    | hd::tl -> if tuple_one hd = p then 1 + calcul tl p
        else calcul tl p in
  (* 0 *)
  let newg = more_tran rel in
  
  calcul newg p

(*
let r1 : (string * string) list = [("A","B");("B","A");("B","E");("D","B");("D","E");("F","F");("A","G");("G","B");("A","E");("E","E");] 
             
let equals v1 v2 = 
  v1 = v2

let test t1 t2 answer =
  let v = likes t1 t2 in
  (equals v answer) 
*)