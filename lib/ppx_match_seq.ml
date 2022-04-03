open Ppxlib

module A = Ast_builder.Default

let merge_loc loc1 loc2 =
  let loc_start = Location.min_pos loc1.loc_start loc2.loc_start in
  let loc_end = Location.max_pos loc1.loc_end loc2.loc_end in
  let loc_ghost = loc1.loc_ghost || loc2.loc_ghost in
  {Location.loc_start; loc_end; loc_ghost}

let loc_of_case {pc_lhs; pc_guard = _; pc_rhs} =
  merge_loc pc_lhs.ppat_loc pc_rhs.pexp_loc

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
  | Ppat_extension ({txt = "seq"; loc = _}, PPat (p, guard)) ->
    expand_pattern p ?guard e
  | _ ->
    expand_pattern p e

let expand_case0 p e =
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

let expand_case0 {pc_lhs; pc_guard; pc_rhs} =
  begin match pc_guard with
  | None -> ()
  | Some {pexp_loc = loc; _} ->
    Location.raise_errorf ~loc "Top-level guards are not allowed."
  end;
  expand_case0 pc_lhs pc_rhs

let rec expand_cases = function
  | [] ->
    assert false
  | case :: [] ->
    expand_case0 case
  | case :: cases ->
    let case_ok =
      let dummy_var = "__x" in
      let lhs =
        let loc = loc_none in
        A.ppat_alias ~loc (psome ~loc (A.ppat_any ~loc)) (Loc.make ~loc dummy_var)
      in
      let rhs = A.evar ~loc:loc_none dummy_var in
      A.case ~lhs ~guard:None ~rhs
    in
    let case_fail = A.case ~lhs:(pnone ~loc:loc_none) ~guard:None ~rhs:(expand_cases cases) in
    let loc = List.fold_left (fun loc case -> merge_loc loc (loc_of_case case)) (loc_of_case case) cases in
    A.pexp_match ~loc (expand_case0 case) [case_ok; case_fail]

let expand ~ctxt:_ e =
  match e.pexp_desc with
  | Pexp_function cases ->
    A.eabstract ~loc:e.pexp_loc [A.pvar ~loc:loc_none __seq]
      (expand_cases cases)
  | Pexp_match (e, cases) ->
    A.pexp_let ~loc:e.pexp_loc Nonrecursive [A.value_binding ~loc:e.pexp_loc ~pat:(A.pvar ~loc:loc_none __seq) ~expr:e]
      (expand_cases cases)
  | _ ->
    Location.raise_errorf ~loc:e.pexp_loc "Invalid payload."

let ppx_match_seq =
  Extension.V3.declare "seq" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand

let rule =
  Context_free.Rule.extension ppx_match_seq

let () =
  Driver.register_transformation ~rules:[ rule ] "ppx_match_seq"
