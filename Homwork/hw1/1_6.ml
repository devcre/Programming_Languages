type btree = Empty | Node of int* btree * btree
                             
let rec notexists n t =
  match t with
  | Empty -> true
  | Node(value, l, r) -> if value = n then false 
      else (notexists n l) && (notexists n r)