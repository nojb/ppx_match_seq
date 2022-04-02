open Lexuniv

type proposition =
  | Variable of string
  | True
  | False
  | Not of proposition
  | And of proposition * proposition
  | Or of proposition * proposition
  | Implies of proposition * proposition
  | Equivalent of proposition * proposition

let read_operator read_operator read_base constructor =
  let rec parse_rest e1 = function%seq
    | [%seq let _ = read_operator] :: [%seq let e2 = read_base] :: [%seq let e = parse_rest (constructor e1 e2)] :: _ -> e
    | _ -> e1
  in
  function%seq [%seq let e1 = read_base] :: [%seq let e = parse_rest e1] :: _ -> e

let rec proposition seq =
  proposition5 seq

and proposition0 = function%seq
  | Ident s :: _ -> Variable s
  | Kwd "true" :: _ -> True
  | Kwd "false" :: _ -> False
  | Kwd "(" :: [%seq let p = proposition] :: Kwd ")" :: _ -> p

and proposition1 = function%seq
  | Kwd "not" :: [%seq let p = proposition0] :: _ -> Not p
  | [%seq let p = proposition0] :: _ -> p

and proposition2 seq =
  read_operator (function%seq Kwd "and" :: _ -> ()) proposition1 (fun p q -> And (p, q)) seq

and proposition3 seq =
  read_operator (function%seq Kwd "or" :: _ -> ()) proposition2 (fun p q -> Or (p, q)) seq

and proposition4 seq =
  read_operator (function%seq Kwd "=>" :: _ -> ()) proposition3 (fun p q -> Implies (p, q)) seq

and proposition5 seq =
  read_operator (function%seq Kwd "<=>" :: _ -> ()) proposition4 (fun p q -> Equivalent (p, q)) seq

let read_proposition seq =
  match proposition seq with
  | None -> raise Parse_error
  | Some (p, _) -> p

let lexer =
  build_lexer ["true"; "false"; "("; ")"; "not"; "and"; "or"; "=>"; "<=>"]

let parse_proposition str =
  read_proposition (lexer (String.to_seq str))
