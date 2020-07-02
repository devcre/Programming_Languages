let rec repeat str i =
  if i=0 then ""
  else String.concat "" [str;(repeat str (i-1))]