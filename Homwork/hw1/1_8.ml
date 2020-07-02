let rec iter n f =
  let compose f g = fun x -> f(g(x)) in
  if n = 0 then fun x -> x else compose f (iter (n-1) f)
  