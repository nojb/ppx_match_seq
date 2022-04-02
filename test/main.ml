[@@@warning "-32-34-37"]

type tok = A of int | B | C

let rec expr = function%seq
  | A n, [%seq? (B, C)] -> n
  | [%seq let x = expr] -> x
