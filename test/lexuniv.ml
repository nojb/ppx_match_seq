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
