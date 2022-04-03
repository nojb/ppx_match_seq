open Ppxlib

module A = Ast_builder.Default

let merge_loc loc1 loc2 =
  let loc_start = Location.min_pos loc1.loc_start loc2.loc_start in
  let loc_end = Location.max_pos loc1.loc_end loc2.loc_end in
  let loc_ghost = loc1.loc_ghost || loc2.loc_ghost in
  {Location.loc_start; loc_end; loc_ghost}

let loc_of_case (p, e) =
  merge_loc p.ppat_loc e.pexp_loc

let mkghost loc =
  {loc with Location.loc_ghost = true}

let pconstr ~loc name args =
  A.ppat_construct ~loc (A.Located.lident ~loc name) args

let econstr ~loc name args =
  A.pexp_construct ~loc (A.Located.lident ~loc name) args

let pnil ~loc = pconstr ~loc "Seq.Nil" None
let pcons ~loc p1 p2 = pconstr ~loc "Seq.Cons" (Some (A.ppat_tuple ~loc:(merge_loc p1.ppat_loc p2.ppat_loc) [p1; p2]))
let esome ~loc e = econstr ~loc "Some" (Some e)
let psome ~loc p = pconstr ~loc "Some" (Some p)
let enone ~loc = econstr ~loc "None" None
let pnone ~loc = pconstr ~loc "None" None

let loc_none = Location.none

let __seq = "__seq"

let expand_pattern p ?guard e =
  A.pexp_match ~loc:(merge_loc p.ppat_loc e.pexp_loc)
    (A.eapply ~loc:loc_none (A.evar ~loc:loc_none __seq) [A.eunit ~loc:loc_none])
    [
      A.case ~lhs:(pcons ~loc:(mkghost p.ppat_loc) p (A.pvar ~loc:loc_none __seq)) ~guard ~rhs:e;
      A.case ~lhs:(A.ppat_any ~loc:loc_none) ~guard:None ~rhs:(enone ~loc:loc_none);
    ]

let expand_case p e =
  match p.ppat_desc with
  | Ppat_extension
      ({txt = "seq"; loc = _}, PStr [{pstr_desc = Pstr_value (Nonrecursive, [{pvb_pat; pvb_expr; _}]); pstr_loc = _}]) ->
    let p_loc_ghost = mkghost pvb_pat.ppat_loc in
    A.pexp_match ~loc:(merge_loc p.ppat_loc e.pexp_loc) (A.eapply ~loc:loc_none pvb_expr [A.evar ~loc:loc_none __seq])
      [
        A.case ~lhs:(psome ~loc:p_loc_ghost (A.ppat_tuple ~loc:p_loc_ghost [pvb_pat; A.pvar ~loc:loc_none __seq]))
          ~guard:None ~rhs:e;
        A.case ~lhs:(pnone ~loc:loc_none) ~guard:None ~rhs:(enone ~loc:loc_none);
      ]
  | Ppat_extension ({txt = "seq"; loc = _}, PStr [{pstr_desc = Pstr_eval (e', _); pstr_loc = _}]) ->
    A.pexp_let ~loc:loc_none Nonrecursive [A.value_binding ~loc:loc_none ~pat:(A.punit ~loc:loc_none) ~expr:e'] e
  | Ppat_extension ({txt = "seq"; loc = _}, PPat (p, guard)) ->
    expand_pattern p ?guard e
  | _ ->
    expand_pattern p e

let expand_case0 (p, e) =
  let e' =
    let loc = mkghost e.pexp_loc in
    esome ~loc (A.pexp_tuple ~loc [e; A.evar ~loc __seq])
  in
  let rec aux p =
    match p.ppat_desc with
    | Ppat_construct ({txt = Lident "::"; loc = _}, Some ([], {ppat_desc = Ppat_tuple [p1; p2]; _})) ->
      expand_case p1 (aux p2)
    | Ppat_any ->
      e'
    | Ppat_construct ({txt = Lident "[]"; loc = nil_loc}, None) ->
      A.pexp_match ~loc:(merge_loc p.ppat_loc e.pexp_loc)
        (A.eapply ~loc:loc_none (A.evar ~loc:loc_none __seq) [A.eunit ~loc:loc_none])
        [
          A.case ~lhs:(pnil ~loc:nil_loc) ~guard:None ~rhs:e';
          A.case ~lhs:(A.ppat_any ~loc:loc_none) ~guard:None ~rhs:(enone ~loc:loc_none);
        ]
    | Ppat_var var ->
      A.pexp_let ~loc:(merge_loc p.ppat_loc e.pexp_loc) Nonrecursive
        [A.value_binding ~loc:var.loc ~pat:p ~expr:(A.evar ~loc:var.loc __seq)] e
    | _ ->
      Location.raise_errorf ~loc:p.ppat_loc "Invalid pattern."
  in
  aux p

let rec expand_cases = function
  | [] ->
    assert false
  | case :: [] ->
    expand_case0 case
  | case :: cases ->
    let case_ok =
      let __x = "__x" in
      let lhs =
        let loc = loc_none in
        A.ppat_alias ~loc (psome ~loc (A.ppat_any ~loc)) (Loc.make ~loc __x)
      in
      let rhs = A.evar ~loc:loc_none __x in
      A.case ~lhs ~guard:None ~rhs
    in
    let case_fail = A.case ~lhs:(pnone ~loc:loc_none) ~guard:None ~rhs:(expand_cases cases) in
    let loc = List.fold_left (fun loc case -> merge_loc loc (loc_of_case case)) (loc_of_case case) cases in
    A.pexp_match ~loc (expand_case0 case) [case_ok; case_fail]

type payload =
  | Function of (pattern * expression) list
  | Match of expression * (pattern * expression) list

let expand ~ctxt:_ = function
  | Function cases ->
    A.eabstract ~loc:loc_none [A.pvar ~loc:loc_none __seq]
      (expand_cases cases)
  | Match (e, cases) ->
    A.pexp_let ~loc:e.pexp_loc Nonrecursive [A.value_binding ~loc:e.pexp_loc ~pat:(A.pvar ~loc:loc_none __seq) ~expr:e]
      (expand_cases cases)

(* Ast_pattern.many is buggy, see: https://github.com/ocaml-ppx/ppxlib/issues/331 *)
let many t =
  let f ctx loc x k =
    let t = Ast_pattern.to_func t ctx loc in
    let rec aux accu = function
      | [] -> k (List.rev accu)
      | x :: xs ->
        t x (fun x -> aux (x :: accu) xs)
    in
    aux [] x
  in
  Ast_pattern.of_func f

let ppx_match_seq =
  let pattern_cases = many Ast_pattern.(pack2 (case ~lhs:__ ~guard:none ~rhs:__)) in
  Extension.V3.declare "seq" Extension.Context.expression
    Ast_pattern.(single_expr_payload
                   (alt (map1 ~f:(fun cases -> Function cases) (pexp_function pattern_cases))
                      (map2 ~f:(fun e cases -> Match (e, cases)) (pexp_match __ pattern_cases))))
    expand

let rule =
  Context_free.Rule.extension ppx_match_seq

let () =
  Driver.register_transformation ~rules:[ rule ] "ppx_match_seq"
