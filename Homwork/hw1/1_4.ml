type formula = TRUE | FALSE
             | NOT of formula
             | ANDALSO of formula * formula
             | ORELSE of formula * formula
             | IMPLY of formula * formula
             | LESS of expr * expr
and expr = NUM of int
         | PLUS of expr * expr
         | MINUS of expr * expr 
                    
let rec eval f = 
  let rec toint intf =
    match intf with 
    | NUM a -> a 
    | PLUS (a, b) -> toint a + toint b
    | MINUS (a, b) -> toint a - toint b in
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT i -> not (eval i)
  | ANDALSO (i, j) -> if (eval i = true) && (eval j = true) then true else false
  | ORELSE (i, j) -> if (eval i = true) || (eval j = true) then true else false
  | IMPLY (i, j) -> if (eval i = true) && (eval j = false) then false else true
  | LESS (i, j) -> if (toint i < toint j) then true else false