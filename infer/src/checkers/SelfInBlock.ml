(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

module DomainData = struct
  type self_pointer_kind = SELF | UNCHECKED_STRONG_SELF | CHECKED_STRONG_SELF | WEAK_SELF
  [@@deriving compare]

  let is_self kind = match kind with SELF -> true | _ -> false

  let is_weak_self kind = match kind with WEAK_SELF -> true | _ -> false

  let is_unchecked_strong_self kind = match kind with UNCHECKED_STRONG_SELF -> true | _ -> false

  let pp_self_pointer_kind fmt kind =
    let s =
      match kind with
      | CHECKED_STRONG_SELF ->
          "CHECKED_STRONG_SELF"
      | SELF ->
          "SELF"
      | UNCHECKED_STRONG_SELF ->
          "UNCHECKED_STRONG_SELF"
      | WEAK_SELF ->
          "WEAK_SELF"
    in
    F.fprintf fmt "%s" s


  let kind_join kind1 kind2 =
    if phys_equal kind1 kind2 then kind1
    else
      match (kind1, kind2) with
      | CHECKED_STRONG_SELF, _ | _, CHECKED_STRONG_SELF ->
          CHECKED_STRONG_SELF
      | _ ->
          Logging.die InternalError
            "The only unequal kinds that can be joined in this domain are CHECKED_STRONG_SELF and \
             UNCHECKED_STRONG_SELF, but found %a, %a"
            pp_self_pointer_kind kind1 pp_self_pointer_kind kind2


  let kind_leq kind1 kind2 =
    if phys_equal kind1 kind2 then true
    else
      match (kind1, kind2) with
      | CHECKED_STRONG_SELF, UNCHECKED_STRONG_SELF ->
          false
      | UNCHECKED_STRONG_SELF, CHECKED_STRONG_SELF ->
          true
      | _ ->
          Logging.die InternalError
            "The only unequal kinds that can be compared in this domain are CHECKED_STRONG_SELF \
             and UNCHECKED_STRONG_SELF, but found %a, %a"
            pp_self_pointer_kind kind1 pp_self_pointer_kind kind2


  type t = {pvar: Pvar.t; typ: Typ.t [@compare.ignore]; loc: Location.t; kind: self_pointer_kind}
  [@@deriving compare]

  let pp fmt {pvar; typ; loc; kind} =
    F.fprintf fmt "%a:%a, at %a (%a)" (Pvar.pp Pp.text) pvar (Typ.pp Pp.text) typ Location.pp loc
      pp_self_pointer_kind kind


  let join elem1 elem2 =
    assert (Pvar.equal elem1.pvar elem2.pvar) ;
    {elem1 with kind= kind_join elem1.kind elem2.kind}


  let widen ~prev ~next ~num_iters:_ = join prev next

  let leq ~lhs ~rhs =
    assert (Pvar.equal lhs.pvar rhs.pvar) ;
    kind_leq lhs.kind rhs.kind
end

module PPPVar = struct
  type t = Pvar.t [@@deriving compare]

  let pp = Pvar.pp Pp.text
end

module CheckedForNull = struct
  type t = {checked: bool; loc: Location.t}

  let pp fmt {checked} =
    let s = match checked with true -> "Checked" | false -> "NotChecked" in
    F.fprintf fmt "%s" s


  let join {checked= elem1; loc= loc1} {checked= elem2; loc= loc2} =
    let loc = match (elem1, elem2) with true, false -> loc2 | false, _ | true, true -> loc1 in
    {checked= AbstractDomain.BooleanAnd.join elem1 elem2; loc}


  let widen ~prev ~next ~num_iters:_ = join prev next

  let leq ~lhs:{checked= lhs} ~rhs:{checked= rhs} = AbstractDomain.BooleanAnd.leq ~lhs ~rhs
end

module StrongEqualToWeakCapturedVars = AbstractDomain.Map (PPPVar) (CheckedForNull)
module Vars = AbstractDomain.Map (Ident) (DomainData)

module Domain = struct
  type t = {vars: Vars.t; strongVars: StrongEqualToWeakCapturedVars.t}

  let pp fmt {vars; strongVars} =
    F.fprintf fmt "%a@.%a" Vars.pp vars StrongEqualToWeakCapturedVars.pp strongVars


  let join lhs rhs =
    { vars= Vars.join lhs.vars rhs.vars
    ; strongVars= StrongEqualToWeakCapturedVars.join lhs.strongVars rhs.strongVars }


  let widen ~prev ~next ~num_iters:_ = join prev next

  let leq ~lhs ~rhs =
    Vars.leq ~lhs:lhs.vars ~rhs:rhs.vars
    && StrongEqualToWeakCapturedVars.leq ~lhs:lhs.strongVars ~rhs:rhs.strongVars
end

module TransferFunctions = struct
  module Domain = Domain
  module CFG = ProcCfg.Normal

  type extras = unit

  let pp_session_name _node fmt = F.pp_print_string fmt "SelfCapturedInBlock"

  let is_captured_strong_self attributes pvar =
    let pvar_name = Pvar.get_name pvar in
    Pvar.is_self pvar
    && List.exists
         ~f:(fun (captured, typ) -> Mangled.equal captured pvar_name && Typ.is_strong_pointer typ)
         attributes.ProcAttributes.captured


  let is_captured_weak_self attributes pvar =
    List.exists
      ~f:(fun (captured, typ) ->
        Mangled.equal captured (Pvar.get_name pvar)
        && String.is_substring ~substring:"self" (String.lowercase (Mangled.to_string captured))
        && Typ.is_weak_pointer typ )
      attributes.ProcAttributes.captured


  let exec_null_check_id (astate : Domain.t) id =
    try
      let elem = Vars.find id astate.vars in
      if StrongEqualToWeakCapturedVars.mem elem.pvar astate.strongVars then
        let strongVarElem = StrongEqualToWeakCapturedVars.find elem.pvar astate.strongVars in
        let strongVars =
          StrongEqualToWeakCapturedVars.add elem.pvar
            {strongVarElem with checked= true}
            astate.strongVars
        in
        let vars =
          (* We may have added UNCHECKED_STRONG_SELF in the previous Load instr,
             but this occurrence is not a bug, since we are checking right here! *)
          Vars.add id {elem with kind= CHECKED_STRONG_SELF} astate.vars
        in
        {Domain.vars; strongVars}
      else astate
    with Not_found_s _ | Caml.Not_found -> astate


  let make_trace_unchecked_strongself (domain : Domain.t) =
    let trace_elems_strongVars =
      StrongEqualToWeakCapturedVars.fold
        (fun pvar {loc} trace_elems ->
          let trace_elem_desc = F.asprintf "%a assigned here" (Pvar.pp Pp.text) pvar in
          let trace_elem = Errlog.make_trace_element 0 loc trace_elem_desc [] in
          trace_elem :: trace_elems )
        domain.strongVars []
    in
    let trace_elems_vars =
      Vars.fold
        (fun _ {pvar; loc; kind} trace_elems ->
          match kind with
          | UNCHECKED_STRONG_SELF ->
              let trace_elem_desc =
                F.asprintf "Using %a not checked for null" (Pvar.pp Pp.text) pvar
              in
              let trace_elem = Errlog.make_trace_element 0 loc trace_elem_desc [] in
              trace_elem :: trace_elems
          | _ ->
              trace_elems )
        domain.vars []
    in
    let trace_elems = List.append trace_elems_strongVars trace_elems_vars in
    List.sort trace_elems ~compare:(fun {Errlog.lt_loc= loc1} {Errlog.lt_loc= loc2} ->
        Location.compare loc1 loc2 )


  let report_unchecked_strongself_issues summary (domain : Domain.t) var_use var =
    try
      let {DomainData.pvar; loc; kind} = Vars.find var domain.vars in
      if DomainData.is_unchecked_strong_self kind then
        let message =
          F.asprintf
            "The variable `%a`, equal to a weak pointer to `self`, is %s without a check for null \
             at %a. This could cause a crash or unexpected behavior."
            (Pvar.pp Pp.text) pvar var_use Location.pp loc
        in
        let ltr = make_trace_unchecked_strongself domain in
        Reporting.log_error summary ~ltr ~loc IssueType.strong_self_not_checked message
    with Not_found_s _ | Caml.Not_found -> ()


  let report_unchecked_strongself_issues_on_exps (domain : Domain.t) summary (instr : Sil.instr) =
    let report_unchecked_strongself_issues_on_exp (exp : Exp.t) =
      match exp with
      | Lfield (Var var, _, _) ->
          report_unchecked_strongself_issues summary domain "dereferenced" var
      | _ ->
          ()
    in
    let exps = Sil.exps_of_instr instr in
    List.iter ~f:report_unchecked_strongself_issues_on_exp exps


  let is_objc_instance proc_desc_opt =
    match proc_desc_opt with
    | Some proc_desc -> (
        let proc_attrs = Procdesc.get_attributes proc_desc in
        match proc_attrs.ProcAttributes.clang_method_kind with
        | ClangMethodKind.OBJC_INSTANCE ->
            true
        | _ ->
            false )
    | None ->
        false


  let get_annotations proc_desc_opt =
    match proc_desc_opt with
    | Some proc_desc ->
        let proc_attrs = Procdesc.get_attributes proc_desc in
        Some proc_attrs.ProcAttributes.method_annotation.params
    | None ->
        None


  let report_unchecked_strongself_issues_on_args (domain : Domain.t) summary pname args =
    let report_issue var =
      report_unchecked_strongself_issues summary domain
        (F.sprintf "passed to %s" (Procname.to_simplified_string pname))
        var
    in
    let rec report_on_non_nullable_arg ?(annots = []) args =
      match (annots, args) with
      | annot :: annot_rest, (Exp.Var var, _) :: rest when not (Annotations.ia_is_nullable annot) ->
          report_issue var ;
          report_on_non_nullable_arg ~annots:annot_rest rest
      | [], (Exp.Var var, _) :: rest ->
          report_issue var ; report_on_non_nullable_arg rest
      | _ ->
          ()
    in
    let proc_desc_opt = Ondemand.get_proc_desc pname in
    let args =
      if is_objc_instance proc_desc_opt then match args with _ :: rest -> rest | [] -> []
      else args
    in
    match get_annotations proc_desc_opt with
    | Some annotations ->
        report_on_non_nullable_arg ~annots:annotations args
    | None ->
        report_on_non_nullable_arg args


  let exec_instr (astate : Domain.t) {ProcData.summary} _cfg_node (instr : Sil.instr) =
    let attributes = Summary.get_attributes summary in
    report_unchecked_strongself_issues_on_exps astate summary instr ;
    match instr with
    | Load {id; e= Lvar pvar; loc; typ} ->
        let vars =
          if is_captured_strong_self attributes pvar then
            Vars.add id {pvar; typ; loc; kind= SELF} astate.vars
          else if is_captured_weak_self attributes pvar then
            Vars.add id {pvar; typ; loc; kind= WEAK_SELF} astate.vars
          else
            try
              let isChecked = StrongEqualToWeakCapturedVars.find pvar astate.strongVars in
              if not isChecked.checked then
                Vars.add id {pvar; typ; loc; kind= UNCHECKED_STRONG_SELF} astate.vars
              else astate.vars
            with Not_found_s _ | Caml.Not_found -> astate.vars
        in
        {astate with vars}
    | Store {e1= Lvar pvar; e2= Var id; typ= pvar_typ; loc} ->
        let strongVars =
          try
            let {DomainData.pvar= binding_for_id} = Vars.find id astate.vars in
            if is_captured_weak_self attributes binding_for_id && Typ.is_strong_pointer pvar_typ
            then StrongEqualToWeakCapturedVars.add pvar {checked= false; loc} astate.strongVars
            else astate.strongVars
          with Not_found_s _ | Caml.Not_found -> astate.strongVars
        in
        {astate with strongVars}
    | Prune (Var id, _, _, _) ->
        exec_null_check_id astate id
    (* If (strongSelf != nil) or equivalent else branch *)
    | Prune (BinOp (Binop.Ne, Var id, e), _, _, _)
    (* If (!(strongSelf == nil)) or equivalent else branch *)
    | Prune (UnOp (LNot, BinOp (Binop.Eq, Var id, e), _), _, _, _) ->
        if Exp.is_null_literal e then exec_null_check_id astate id else astate
    | Call (_, Exp.Const (Const.Cfun callee_pn), args, _, _) ->
        report_unchecked_strongself_issues_on_args astate summary callee_pn args ;
        astate
    | _ ->
        astate
end

let make_trace_use_self_weakself domain =
  let trace_elems =
    Vars.fold
      (fun _ {pvar; loc; kind} trace_elems ->
        match kind with
        | SELF | WEAK_SELF ->
            let trace_elem_desc = F.asprintf "Using %a" (Pvar.pp Pp.text) pvar in
            let trace_elem = Errlog.make_trace_element 0 loc trace_elem_desc [] in
            trace_elem :: trace_elems
        | _ ->
            trace_elems )
      domain []
  in
  List.sort trace_elems ~compare:(fun {Errlog.lt_loc= loc1} {Errlog.lt_loc= loc2} ->
      Location.compare loc1 loc2 )


let report_mix_self_weakself_issues summary domain =
  let weakSelf_opt =
    Vars.filter (fun _ {kind} -> DomainData.is_weak_self kind) domain |> Vars.choose_opt
  in
  let self_opt = Vars.filter (fun _ {kind} -> DomainData.is_self kind) domain |> Vars.choose_opt in
  match (weakSelf_opt, self_opt) with
  | Some (_, {pvar= weakSelf; loc= weakLoc}), Some (_, {pvar= self; loc= selfLoc}) ->
      let message =
        F.asprintf
          "This block uses both `%a` (%a) and `%a` (%a). This could lead to retain cycles or \
           unexpected behavior."
          (Pvar.pp Pp.text) weakSelf Location.pp weakLoc (Pvar.pp Pp.text) self Location.pp selfLoc
      in
      let ltr = make_trace_use_self_weakself domain in
      Reporting.log_error summary ~ltr ~loc:selfLoc IssueType.mixed_self_weakself message
  | _ ->
      ()


let report_weakself_multiple_issues summary domain =
  let weakSelfs =
    Vars.filter (fun _ {kind} -> DomainData.is_weak_self kind) domain
    |> Vars.bindings |> List.map ~f:snd
  in
  let sorted_WeakSelfs =
    List.sort weakSelfs ~compare:(fun {DomainData.loc= loc1} {DomainData.loc= loc2} ->
        Location.compare loc1 loc2 )
  in
  match sorted_WeakSelfs with
  | {pvar= weakSelf; loc= weakLoc1} :: {loc= weakLoc2} :: _ ->
      let message =
        F.asprintf
          "This block uses the weak pointer `%a` more than once (%a) and (%a). This could lead to \
           unexpected behavior. Even if `%a` is not nil in the first use, it could be nil in the \
           following uses since the object that `%a` points to could be freed anytime; assign it \
           to a strong variable first."
          (Pvar.pp Pp.text) weakSelf Location.pp weakLoc1 Location.pp weakLoc2 (Pvar.pp Pp.text)
          weakSelf (Pvar.pp Pp.text) weakSelf
      in
      let ltr = make_trace_use_self_weakself domain in
      Reporting.log_error summary ~ltr ~loc:weakLoc1 IssueType.multiple_weakself message
  | _ ->
      ()


module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

let checker {Callbacks.exe_env; summary} =
  let initial = {Domain.vars= Vars.empty; strongVars= StrongEqualToWeakCapturedVars.empty} in
  let procname = Summary.get_proc_name summary in
  let tenv = Exe_env.get_tenv exe_env procname in
  let proc_data = ProcData.make summary tenv () in
  ( if Procname.is_objc_block procname then
    match Analyzer.compute_post proc_data ~initial with
    | Some domain ->
        report_mix_self_weakself_issues summary domain.vars ;
        report_weakself_multiple_issues summary domain.vars
    | None ->
        () ) ;
  summary
