open Ppxlib

module A = Ast_builder.Default

let loc = Location.none

let pcons pcar pcdr =
  A.ppat_construct ~loc (A.Located.lident ~loc "Seq.Cons")
    (Some (A.ppat_tuple ~loc [pcar; pcdr]))

let esome e =
  A.pexp_construct ~loc (A.Located.lident ~loc "Some") (Some e)

let psome p =
  A.ppat_construct ~loc (A.Located.lident ~loc "Some") (Some p)

let enone =
  A.pexp_construct ~loc (A.Located.lident ~loc "None") None

let pnone =
  A.ppat_construct ~loc (A.Located.lident ~loc "None") None

let expand_pattern p ?guard e =
  A.pexp_match ~loc (A.eapply ~loc (A.evar ~loc "__seq") [A.eunit ~loc])
    [
      A.case ~lhs:(pcons p (A.pvar ~loc "__seq")) ~guard ~rhs:e;
      A.case ~lhs:(A.pvar ~loc "_") ~guard:None ~rhs:enone;
    ]

(* shoudl expand to an expression of type a seq -> (b * seq) option *)
let expand_case p e =
  match p.ppat_desc with
  | Ppat_extension ({txt = "seq"; loc = _}, PStr [{pstr_desc = Pstr_value (Nonrecursive, [{pvb_pat; pvb_expr; _}]);
                                                   pstr_loc = _}]) ->
    A.pexp_match ~loc (A.eapply ~loc pvb_expr [A.evar ~loc "__seq"])
      [
        A.case ~lhs:(psome (A.ppat_tuple ~loc [pvb_pat; A.pvar ~loc "__seq"])) ~guard:None
          ~rhs:e;
        A.case ~lhs:pnone ~guard:None ~rhs:enone;
      ]
  | Ppat_extension ({txt = "seq"; loc = _}, PPat (p, guard)) ->
    expand_pattern p ?guard e
  | _ ->
    expand_pattern p e

let rec expand_case0 p e =
  match p.ppat_desc with
  | Ppat_construct ({txt = Lident "::"; _}, Some ([], {ppat_desc = Ppat_tuple [p1; p2]; _})) ->
    expand_case p1 (expand_case0 p2 e)
  | _ ->
    expand_case p e

let expand_case0 {pc_lhs; pc_guard = _; pc_rhs} =
  let pc_rhs = esome (A.pexp_tuple ~loc [pc_rhs; A.evar ~loc "__seq"]) in
  expand_case0 pc_lhs pc_rhs

let rec expand_cases = function
  | [] -> assert false
  | [case] -> expand_case0 case
  | case :: cases ->
    A.pexp_match ~loc (expand_case0 case)
      [
        A.case ~lhs:(A.ppat_alias ~loc (psome (A.pvar ~loc "_")) (Loc.make ~loc "__x")) ~guard:None ~rhs:(A.evar ~loc "__x");
        A.case ~lhs:pnone ~guard:None ~rhs:(expand_cases cases)
      ]

let seq_var = "__seq"

let expand ~ctxt:_ e =
  match e.pexp_desc with
  | Pexp_function cases ->
    A.eabstract ~loc [A.pvar ~loc seq_var]
      (expand_cases cases)
  | Pexp_match (e, cases) ->
    A.pexp_let ~loc Nonrecursive [A.value_binding ~loc ~pat:(A.pvar ~loc seq_var) ~expr:e]
      (expand_cases cases)
  | _ -> assert false

let ppx_match_seq =
  Extension.V3.declare "seq" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand

let rule =
  Context_free.Rule.extension ppx_match_seq

let () =
  Driver.register_transformation ~rules:[ rule ] "ppx_match_seq"
