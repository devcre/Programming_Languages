let rec uniq alist =
  let rec removeall l x =
    match l with
    | [] -> []
    | h :: t -> if x = h then removeall t x
        else h :: removeall t x in
  match alist with
  | [] -> []
  | h :: t -> h :: uniq (removeall t h)

let equals v1 v2 = 
  v1 = v2

let test t1 answer =
  let v = uniq t1 in
  (equals v answer)