[@@@warning "-32-34-37"]

type tok = A of int | B | C

let rec expr = fun seq ->
  match%seq seq with
  | A n :: [%seq? B when false] :: C :: [%seq let x = expr] :: [%seq let y = expr] :: _ -> x + y + n
  | C :: C :: C :: _ -> 0
  | [%seq let x = expr] :: _ -> x + 1
  | [A 12; B] -> 32
