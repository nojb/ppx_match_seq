type proposition =
  | Variable of string
  | True
  | False
  | Not of proposition
  | And of proposition * proposition
  | Or of proposition * proposition
  | Implies of proposition * proposition
  | Equivalent of proposition * proposition

val parse_proposition: string -> proposition
