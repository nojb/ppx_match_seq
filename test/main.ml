[@@@warning "-32-34-37"]

type tok = A of int | B | C

let rec expr = fun seq ->
  match%seq seq with
  | A n :: [%seq? B when false] -> n
  | [%seq let x = expr] -> x
