let rec rev_append l1 l2 =
  match l1 with
  |[] -> l2
  |h1::t1 -> rev_append t1 (h1::l2)