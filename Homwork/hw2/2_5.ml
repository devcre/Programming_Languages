let rec count_string s x =
  if String.length s < String.length x then 0
  else if x = "" then 0
  else if String.sub s 0 (String.length x) = x
  then 1 + count_string (String.sub s 1 ((String.length s)-1)) x
  else count_string (String.sub s 1 ((String.length s)-1)) x
      
let equals v1 v2 =
  v1 = v2

let test t1 t2 answer =
  let v = count_string t1 t2 in
  (equals v answer) 
