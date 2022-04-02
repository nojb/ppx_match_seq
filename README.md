## `ppx_match_seq`

We describe the syntax and the semantics of the parsers of sequences `'a
Seq.t`. Parsers defined with this syntax are normal functions of type `'a Seq.t
-> ('b * Seq.t) option`, where `'a` is the type of the elements of the sequence
and `'b` the type of the result of the parser. Parsers are specified by a
sequence of cases which are tested in the order they are defined until one of
them applies (backtracking).

Parsers are just syntactic sugar: the syntax is transformed into normal function
calls and pattern matchings.

### Syntax

The syntax of parsers is the following:

```
          parser ::= "function%seq" parser-cases
                   | "match%seq" expression "with" parser-cases

    parser-cases ::= parser-cases parser-case
                   | <nothing>

     parser-case ::= seq-pattern-cont "->" expression

     seq-pattern ::= pattern
                   | "[%seq" "let" pattern "=" expression "]"
                   | "[%seq?" pattern ["when" expression] "]"

seq-pattern-cont ::= "_"
                   | "[]"
                   | variable
                   | seq-pattern "::" seq-pattern-cont
```

The relation between `match%seq` and `function%seq` is the same as that between
`match` and `function`.

A parser consist of a sequence of cases `parser-case`. Each case consist of a
sequence of components `seq-pattern`. Each component other than the final one
matches an element of the sequence, in order. The final component is special as
it "matches" the rest of the sequence (see semantics below).

If a sequence matches one whole pattern case, then the associated *semantic
action* (the expressions on the right of the arrows `->`) is evaluated.

### Semantics

A parser (as defined by the `parser` rule above) is a function `'a Seq.t -> ('b * 'a Seq.t) option`
where `'a` is the type of the elements of the input sequence
and `'b` is the type of the result of the parser.

The parser is defined by a sequence of "parser-cases". These are tested in in
order until one of them applies.

Inside a single parser case, the sequence components are separated with the `::`
constructor and consist of a sequence of *components*, ending with a final
component. The possibilities for components other than the final one are as
follows:

- a single pattern `p`, means: if the sequence starts with an element which is
  matched by this pattern, the component matches, and the remaining sequence is
  matched with the rest of the pattern case.

- an extension node `[%seq let p = e]` where `p` is a pattern of type `'b` and
  `e` is expression of type `'a Seq.t -> ('b * 'a Seq.t) option` means: call the
  expression (which is a parser) with the current sequence. If this sub-parser
  succeeds, the pattern is bound to its result and the next component is tested.

- an extension node `[%seq p]` where `p` is a pattern, means: the first element
  of the sequence is matched against `p` and if it succeeds, the next component
  is tested.

- an extension node `[%seq p when e]` where `p` is a pattern and `e` is an
  expression of type `bool`, means: the first element of the sequence is matched
  against `p`; if it matches and `e` evalues to `true`, then the whole component
  matches and the rest of the sequence is tested against the remaining
  components.

For the final component, the possibilities are as follows:

- the empty list constructor, `[]`, which means: the current sequence matches if
  it contains no remaining elements.

- the wildcard `_`, means: the type of the semantic action is `'b`, if the
  sequence matches this case, the semantic action is evaluated giving some
  result `e` and the result of the parser is `Some (e, seq)` where `seq` is the
  remaining stream.

- a pattern variable `var`, means: the remaining sequence is bound to `var` and
  the type of the semantic action is `('b * 'a Seq.t) option`, and its result is
  also the result of the whole parser.

Notice that patterns are bound immediately and can be used in the next pattern
component.

### Quickstart

**Requirements**

- `dune`
- `ppxlib`
- `utop` (optional)

To play with the syntax, use

```
make top
```

This will launch `utop` with the syntax already loaded in it. Try doing for example

```
# let p = function%seq 3 :: 1 :: 4 :: _ -> "hey";;
val p : (unit -> int Seq.node) -> (string * int Seq.t) option = <fun>
# p (List.to_seq [3; 1; 4]);;
- : (string * int Seq.t) option = Some ("hey", <fun>)
# p (List.to_seq [3; 1; 4; 1; 5; 9]);;
- : (string * int Seq.t) option = Some ("hey", <fun>)
# p (List.to_seq [1; 1; 4]);;
- : (string * int Seq.t) option = None
```

or

```
# type tok = If | Then | Else | Let | In | Equal | Ident of int;;
# let rec expr = function%seq
    | If :: [%seq let x = expr] :: Then :: [%seq let y = expr] :: Else :: [%seq let z = expr] :: _ -> "if"
    | Let :: Ident x :: Equal :: [%seq let x = expr] :: In :: [%seq let y = expr] :: _ -> "let";;
val expr : tok Seq.t -> (string * (unit -> tok Seq.node)) option = <fun>
```

If you want to inspect the generated code, you can use
```
make top-dsource
```

### A larger example

See the [Demo.](demo/)

### The rewriting

The second example above is desugared as follows:

```ocaml
let rec expr __seq =
  match match __seq () with
        | Seq.Cons (If, __seq) ->
            (match expr __seq with
             | Some (x, __seq) ->
                 (match __seq () with
                  | Seq.Cons (Then, __seq) ->
                      (match expr __seq with
                       | Some (y, __seq) ->
                           (match __seq () with
                            | Seq.Cons (Else, __seq) ->
                                (match expr __seq with
                                 | Some (z, __seq) -> Some ("if", __seq)
                                 | None -> None)
                            | _ -> None)
                       | None -> None)
                  | _ -> None)
             | None -> None)
        | _ -> None
  with
  | Some _ as x -> x
  | None ->
      (match __seq () with
       | Seq.Cons (Let, __seq) ->
           (match __seq () with
            | Seq.Cons (Ident x, __seq) ->
                (match __seq () with
                 | Seq.Cons (Equal, __seq) ->
                     (match expr __seq with
                      | Some (x, __seq) ->
                          (match __seq () with
                           | Seq.Cons (In, __seq) ->
                               (match expr __seq with
                                | Some (y, __seq) -> Some ("let", __seq)
                                | None -> None)
                           | _ -> None)
                      | None -> None)
                 | _ -> None)
            | _ -> None)
       | _ -> None);;
```
