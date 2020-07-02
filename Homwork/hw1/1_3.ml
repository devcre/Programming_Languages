let partition p l = 
  let rec fun1 p list =
    match list with
    | [] -> []
    | h::t -> if (p h) = true then h::(fun1 p t) else fun1 p t in
  let rec fun2 p list =
    match list with
    | [] -> []
    | h::t -> if (p h) = true then fun2 p t else h::(fun2 p t) in
  (fun1 p l,fun2 p l)
                    