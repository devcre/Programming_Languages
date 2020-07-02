let selflove rel =
  let tuple_one tu =
    match tu with
    | (e, _) -> e 
  in
  
  let tuple_two tu =
    match tu with
    | (_, e) -> e 
  in
  
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
  
  let newrel = more_tran rel in
  
  let rec calcul rel =
    match rel with
    | [] -> 0
    | hd::tl -> if (tuple_one hd) = (tuple_two hd)
        then 1 + calcul tl
        else calcul tl in
  
  calcul newrel