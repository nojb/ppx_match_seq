open Asynt

let examples =
  [
    "not (not P) <=> P",
    Equivalent (Not (Not (Variable "P")), Variable "P");
    "P or (not P) <=> true",
    Equivalent (Or (Variable "P", Not (Variable "P")), True);
    "P and (not P) <=> false",
    Equivalent (And (Variable "P", Not (Variable "P")), False);
    "(P <=> true) <=> P",
    Equivalent (Equivalent (Variable "P", True), Variable "P");
    "(P <=> false) <=> not P",
    Equivalent (Equivalent (Variable "P", False), Not (Variable "P"));
    "P or (not P)",
    Or (Variable "P", Not (Variable "P"));
    "not (P and (not P))",
    Not (And (Variable "P", Not (Variable "P")));
    "P or P <=> P",
    Equivalent (Or (Variable "P", Variable "P"), Variable "P");
    "P and P <=> P",
    Equivalent (And (Variable "P", Variable "P"), Variable "P");
    "P => P",
    Implies (Variable "P", Variable "P");
    "P <=> P",
    Equivalent (Variable "P", Variable "P");
    "(P <=> Q) <=> (Q <=> P)",
    Equivalent (Equivalent (Variable "P", Variable "Q"),
                Equivalent (Variable "Q", Variable "P"));
    "(not P <=> not Q) <=> (P <=> Q)",
    Equivalent (Equivalent (Not (Variable "P"), Not (Variable "Q")),
                Equivalent (Variable "P", Variable "Q"));
    "(P => Q) <=> (not Q => not P)",
    Equivalent (Implies (Variable "P", Variable "Q"),
                Implies (Not (Variable "Q"), Not (Variable "P")));
    "(P and not Q) => not (P and Q)",
    Implies (And (Variable "P", Not (Variable "Q")),
             Not (And (Variable "P", Variable "Q")));
  ]

let () =
  List.iter (fun (str, expected) -> assert (parse_proposition str = expected)) examples
