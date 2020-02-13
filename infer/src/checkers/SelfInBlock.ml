(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

module DomainData = struct
  type self_pointer_kind =
    | CAPTURED_STRONG_SELF
    | CHECKED_STRONG_SELF
    | SELF
    | UNCHECKED_STRONG_SELF
    | WEAK_SELF
  [@@deriving compare]

  let is_unchecked_strong_self kind = match kind with UNCHECKED_STRONG_SELF -> true | _ -> false

  let pp_self_pointer_kind fmt kind =
    let s =
      match kind with
      | CAPTURED_STRONG_SELF ->
          "CAPTURED_STRONG_SELF"
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

  let is_captured_self attributes pvar =
    let pvar_name = Pvar.get_name pvar in
    Pvar.is_self pvar
    && List.exists
         ~f:(fun (captured, typ) -> Mangled.equal captured pvar_name && Typ.is_strong_pointer typ)
         attributes.ProcAttributes.captured


  (* The variable is captured in the block, contains self in the name, is not self, and it's strong. *)
  let is_captured_strong_self attributes pvar =
    (not (Pvar.is_self pvar))
    && List.exists
         ~f:(fun (captured, typ) ->
           Typ.is_strong_pointer typ
           && Mangled.equal captured (Pvar.get_name pvar)
           && String.is_suffix ~suffix:"self" (String.lowercase (Mangled.to_string captured)) )
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
    with Caml.Not_found -> astate


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
    with Caml.Not_found -> ()


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


  (* The translation of closures includes a load instruction for the captured variable,
     then we add that corresponding id to the closure. This doesn't correspond to an
     actual "use" of the captured variable in the source program though, and causes false
     positives. Here we remove the ids from the domain when that id is being added to a closure. *)
  let remove_ids_in_closures_from_domain (domain : Domain.t) (instr : Sil.instr) =
    let remove_id_in_closures_from_domain vars ((exp : Exp.t), _, _) =
      match exp with Var id -> Vars.remove id vars | _ -> vars
    in
    let do_exp vars (exp : Exp.t) =
      match exp with
      | Closure {captured_vars} ->
          List.fold ~init:vars ~f:remove_id_in_closures_from_domain captured_vars
      | _ ->
          vars
    in
    let exps = Sil.exps_of_instr instr in
    let vars = List.fold ~init:domain.vars ~f:do_exp exps in
    {domain with vars}


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
        (F.sprintf "passed to `%s`" (Procname.to_simplified_string pname))
        var
    in
    let rec report_on_non_nullable_arg ?annotations args =
      match (annotations, args) with
      | Some (annot :: annot_rest), (arg, _) :: rest ->
          ( match arg with
          | Exp.Var var when not (Annotations.ia_is_nullable annot) ->
              report_issue var
          | _ ->
              () ) ;
          report_on_non_nullable_arg ~annotations:annot_rest rest
      | None, (arg, _) :: rest ->
          (match arg with Exp.Var var -> report_issue var | _ -> ()) ;
          report_on_non_nullable_arg rest
      | _ ->
          ()
    in
    let proc_desc_opt = Ondemand.get_proc_desc pname in
    let annotations = get_annotations proc_desc_opt in
    let args =
      if is_objc_instance proc_desc_opt then match args with _ :: rest -> rest | [] -> []
      else args
    in
    let annotations =
      if is_objc_instance proc_desc_opt then
        match annotations with Some (_ :: rest) -> Some rest | _ -> annotations
      else annotations
    in
    report_on_non_nullable_arg ?annotations args


  let exec_instr (astate : Domain.t) {ProcData.summary} _cfg_node (instr : Sil.instr) =
    let attributes = Summary.get_attributes summary in
    report_unchecked_strongself_issues_on_exps astate summary instr ;
    let astate = remove_ids_in_closures_from_domain astate instr in
    match instr with
    | Load {id; e= Lvar pvar; loc; typ} ->
        let vars =
          if is_captured_self attributes pvar then
            Vars.add id {pvar; typ; loc; kind= SELF} astate.vars
          else if is_captured_strong_self attributes pvar then
            Vars.add id {pvar; typ; loc; kind= CAPTURED_STRONG_SELF} astate.vars
          else if is_captured_weak_self attributes pvar then
            Vars.add id {pvar; typ; loc; kind= WEAK_SELF} astate.vars
          else
            try
              let isChecked = StrongEqualToWeakCapturedVars.find pvar astate.strongVars in
              if not isChecked.checked then
                Vars.add id {pvar; typ; loc; kind= UNCHECKED_STRONG_SELF} astate.vars
              else astate.vars
            with Caml.Not_found -> astate.vars
        in
        {astate with vars}
    | Store {e1= Lvar pvar; e2= Var id; typ= pvar_typ; loc} ->
        let strongVars =
          try
            let {DomainData.pvar= binding_for_id} = Vars.find id astate.vars in
            if is_captured_weak_self attributes binding_for_id && Typ.is_strong_pointer pvar_typ
            then StrongEqualToWeakCapturedVars.add pvar {checked= false; loc} astate.strongVars
            else astate.strongVars
          with Caml.Not_found -> astate.strongVars
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


let make_trace_captured_strong_self domain =
  let trace_elems =
    Vars.fold
      (fun _ {pvar; loc; kind} trace_elems ->
        match kind with
        | CAPTURED_STRONG_SELF ->
            let trace_elem_desc = F.asprintf "Using captured %a" (Pvar.pp Pp.text) pvar in
            let trace_elem = Errlog.make_trace_element 0 loc trace_elem_desc [] in
            trace_elem :: trace_elems
        | _ ->
            trace_elems )
      domain []
  in
  List.sort trace_elems ~compare:(fun {Errlog.lt_loc= loc1} {Errlog.lt_loc= loc2} ->
      Location.compare loc1 loc2 )


let report_mix_self_weakself_issues summary domain (weakSelf : DomainData.t) (self : DomainData.t) =
  let message =
    F.asprintf
      "This block uses both `%a` (%a) and `%a` (%a). This could lead to retain cycles or \
       unexpected behavior."
      (Pvar.pp Pp.text) weakSelf.pvar Location.pp weakSelf.loc (Pvar.pp Pp.text) self.pvar
      Location.pp self.loc
  in
  let ltr = make_trace_use_self_weakself domain in
  Reporting.log_error summary ~ltr ~loc:self.loc IssueType.mixed_self_weakself message


let report_weakself_in_no_escape_block_issues summary domain (weakSelf : DomainData.t) procname =
  let message =
    F.asprintf
      "This block uses `%a` at %a. This is probably not needed since the block is passed to the \
       method `%s` in a position annotated with NS_NOESCAPE. Use `self` instead."
      (Pvar.pp Pp.text) weakSelf.pvar Location.pp weakSelf.loc
      (Procname.to_simplified_string procname)
  in
  let ltr = make_trace_use_self_weakself domain in
  Reporting.log_error summary ~ltr ~loc:weakSelf.loc IssueType.weak_self_in_noescape_block message


let report_weakself_multiple_issue summary domain (weakSelf1 : DomainData.t)
    (weakSelf2 : DomainData.t) =
  let message =
    F.asprintf
      "This block uses the weak pointer `%a` more than once (%a) and (%a). This could lead to \
       unexpected behavior. Even if `%a` is not nil in the first use, it could be nil in the \
       following uses since the object that `%a` points to could be freed anytime; assign it to a \
       strong variable first."
      (Pvar.pp Pp.text) weakSelf1.pvar Location.pp weakSelf1.loc Location.pp weakSelf2.loc
      (Pvar.pp Pp.text) weakSelf1.pvar (Pvar.pp Pp.text) weakSelf1.pvar
  in
  let ltr = make_trace_use_self_weakself domain in
  Reporting.log_error summary ~ltr ~loc:weakSelf1.loc IssueType.multiple_weakself message


let report_captured_strongself_issue domain summary (capturedStrongSelf : DomainData.t) =
  let message =
    F.asprintf
      "The variable `%a`, used at `%a`, is a strong pointer to `self` captured in this block. This \
       could lead to retain cycles or unexpected behavior since to avoid retain cycles one usually \
       uses a local strong pointer or a captured weak pointer instead."
      (Pvar.pp Pp.text) capturedStrongSelf.pvar Location.pp capturedStrongSelf.loc
  in
  let ltr = make_trace_captured_strong_self domain in
  Reporting.log_error summary ~ltr ~loc:capturedStrongSelf.loc IssueType.captured_strong_self
    message


let report_issues summary domain attributes =
  let process_domain_item _ (domain_data : DomainData.t) (weakSelfList, selfList) =
    match domain_data.kind with
    | DomainData.CAPTURED_STRONG_SELF ->
        report_captured_strongself_issue domain summary domain_data ;
        (weakSelfList, selfList)
    | DomainData.WEAK_SELF ->
        ( match attributes.ProcAttributes.passed_as_noescape_block_to with
        | Some procname ->
            report_weakself_in_no_escape_block_issues summary domain domain_data procname
        | None ->
            () ) ;
        (domain_data :: weakSelfList, selfList)
    | DomainData.SELF ->
        (weakSelfList, domain_data :: selfList)
    | _ ->
        (weakSelfList, selfList)
  in
  let weakSelfList, selfList = Vars.fold process_domain_item domain ([], []) in
  let sorted_WeakSelfList =
    List.sort weakSelfList ~compare:(fun {DomainData.loc= loc1} {DomainData.loc= loc2} ->
        Location.compare loc1 loc2 )
  in
  let sorted_selfList =
    List.sort selfList ~compare:(fun {DomainData.loc= loc1} {DomainData.loc= loc2} ->
        Location.compare loc1 loc2 )
  in
  ( match (sorted_WeakSelfList, sorted_selfList) with
  | weakSelf :: _, self :: _ ->
      report_mix_self_weakself_issues summary domain weakSelf self
  | _ ->
      () ) ;
  match sorted_WeakSelfList with
  | weakSelf1 :: weakSelf2 :: _ ->
      report_weakself_multiple_issue summary domain weakSelf1 weakSelf2
  | _ ->
      ()


module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

let checker {Callbacks.exe_env; summary} =
  let initial = {Domain.vars= Vars.empty; strongVars= StrongEqualToWeakCapturedVars.empty} in
  let procname = Summary.get_proc_name summary in
  let tenv = Exe_env.get_tenv exe_env procname in
  let proc_data = ProcData.make summary tenv () in
  let attributes = Summary.get_attributes summary in
  ( if Procname.is_objc_block procname then
    match Analyzer.compute_post proc_data ~initial with
    | Some domain ->
        report_issues summary domain.vars attributes
    | None ->
        () ) ;
  summary
