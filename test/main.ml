exception Parse_error

type lexeme =
  | Kwd of string
  | Ident of string
  | Int of int

let rec read_integer accum seq =
  match%seq seq with
  | ('0'..'9' as c) :: seq -> read_integer (10 * accum + int_of_char c - 48) seq
  | _ -> accum

let stamp = Bytes.make 16 '-'

let rec read_word position seq =
  match%seq seq with
  | ('A'..'Z'|'a'..'z'|'0'..'9'|'_'|'\'' as c) :: seq ->
    if position < Bytes.length stamp then
      Bytes.set stamp position c;
    read_word (position + 1) seq
  | _ ->
    Bytes.sub_string stamp 0 (min position (Bytes.length stamp))

let rec read_symbol position seq =
  match%seq seq with
  | ('('|'!'|'$'|'%'|'&'|'*'|'+'|'-'|'.'|'/'|':'|
     ';'|'<'|'='|'>'|'?'|'@'|'^'|'|'|'~' as c) :: seq ->
    if position < Bytes.length stamp then
      Bytes.set stamp position c;
    read_symbol (position + 1) seq
  | _ ->
    Bytes.sub_string stamp 0 (min position (Bytes.length stamp))

let rec read_comment seq =
  match%seq seq with
  | '\n' :: _ -> ()
  | _ :: seq -> read_comment seq

let kwd_or_ident keyword_table ident =
  try Hashtbl.find keyword_table ident
  with Not_found -> Ident ident

let kwd_or_error keyword_table char =
  let ident = String.make 1 char in
  try Hashtbl.find keyword_table ident
  with Not_found -> raise Parse_error

let rec read_lexeme table seq =
  match%seq seq with
  | (' ' | '\n' | '\r' | '\t') :: seq -> read_lexeme table seq
  | '#' :: [%seq let _ = read_comment] :: seq -> read_lexeme table seq
  | ('A'..'Z'|'a'..'z'|'0'..'9'|'_'|'\'' as c) :: seq ->
    Bytes.set stamp 0 c;
    (match%seq seq with [%seq let word = read_word 1] :: _ -> kwd_or_ident table word)
  | ('('|'!'|'$'|'%'|'&'|'*'|'+'|'-'|'.'|'/'|':'|
     ';'|'<'|'='|'>'|'?'|'@'|'^'|'|'|'~' as c) :: seq ->
    Bytes.set stamp 0 c;
    (match%seq seq with [%seq let sym = read_symbol 1] :: _ -> kwd_or_ident table sym)
  | ('0'..'9' as c) :: [%seq let n = read_integer (int_of_char c - 48)] :: _ ->
    Int n
  | '-' :: seq ->
    begin match%seq seq with
    | ('0'..'9' as c) :: [%seq let n = read_integer (int_of_char c - 48)] :: _ ->
      Int (- n)
    | seq ->
      Bytes.set stamp 0 '-';
      (match%seq seq with [%seq let sym = read_symbol 1] :: _ -> kwd_or_ident table sym)
    end
  | c :: _ ->
    kwd_or_error table c

let rec lexer table seq () =
  match seq () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons _ ->
    begin match read_lexeme table seq with
    | None -> raise Parse_error
    | Some (x, seq) -> Seq.Cons (x, lexer table seq)
    end

let build_lexer keywords =
  let keyword_table = Hashtbl.create 17 in
  List.iter (fun word -> Hashtbl.add keyword_table word (Kwd word)) keywords;
  lexer keyword_table

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

let example1 = "not (not P) <=> P"

let example2 = "P or (not P) <=> true"

let examples =
  [example1; example2 ]

let _ =
  List.map parse_proposition examples
