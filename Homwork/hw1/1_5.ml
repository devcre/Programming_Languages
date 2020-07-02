type btree = Empty | Node of int * btree * btree
                             
let rec height tr =
  match tr with
  | Empty -> 0 
  | Node(n, Empty, Empty) -> 1
  | Node(n, l, r) -> if (height l) > (height r) then (height l) + 1 
      else (height r) + 1