let rec cartesian la lb =
  let rec makePair a list = 
    match list with
    | [] -> []
    | h::t -> (a, h)::(makePair a t) in
  match la with
  | [] -> []
  | hd::tl -> (makePair hd lb)@(cartesian tl lb)