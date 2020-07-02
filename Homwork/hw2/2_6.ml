type btree = Empty | Node of int * btree * btree
                               
let rec check btree =
  let rec height t =
    match t with
    | Empty -> 0
    | Node (_, l, r) -> if (height l) > (height r) then (height l) + 1
        else (height r) + 1
  in
  match btree with
  | Empty -> true
  | Node(_, l, r) -> 
      if abs((height r) - (height l)) <= 1
      then (check l) && (check r)
      else false
        
let equals v1 v2 = 
  v1 = v2

let test t1 answer =
  let v = check t1 in
  (equals v answer)