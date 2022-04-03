Here you will find a larger example ported from [*Le langage Caml*](https://caml.inria.fr/pub/distrib/books/llc.pdf),
written by Pierre Weiss and Xavier Leroy. Namely this folder contains the parsing part of chapter 12
(sections 12.5 and 12.8).

- `lexuniv.ml` contains the "universal lexer generator", roughly similar to
  `Genlex` in spirit.

- `asynt.ml` contains a parser for the language of logical propositions.

- `demo.ml` contains a small set of tests of the parser.
