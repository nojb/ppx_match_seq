exception Parse_error

type lexeme =
  | Kwd of string
  | Ident of string
  | Int of int

val build_lexer: string list -> (char Seq.t -> lexeme Seq.t)
