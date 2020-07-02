let rec to_list str =
  match str with
  | "" -> []
  | _ -> (String.get str 0) :: to_list (String.sub str 1 (String.length str-1))