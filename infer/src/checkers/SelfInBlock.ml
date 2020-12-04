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
  type t = {checked: bool; loc: Location.t; reported: bool}

  let pp fmt {checked; reported} =
    let s = match checked with true -> "Checked" | false -> "NotChecked" in
    let s' = match reported with true -> "Reported" | false -> "NotReported" in
    F.fprintf fmt "%s, %s" s s'


  let join {checked= elem1; loc= loc1; reported= reported1}
      {checked= elem2; loc= loc2; reported= reported2} =
    let loc, reported =
      match (elem1, elem2) with true, false -> (loc2, reported2) | _ -> (loc1, reported1)
    in
    {checked= elem1 && elem2; loc; reported}


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

type report_issues_result =
  { reported_captured_strong_self: Pvar.Set.t
  ; reported_weak_self_in_noescape_block: Pvar.Set.t
  ; selfList: DomainData.t list
  ; weakSelfList: DomainData.t list }

module TransferFunctions = struct
  module Domain = Domain
  module CFG = ProcCfg.Normal

  type analysis_data = IntraproceduralAnalysis.t

  let pp_session_name _node fmt = F.pp_print_string fmt "SelfCapturedInBlock"

  let is_captured_self attributes pvar =
    let pvar_name = Pvar.get_name pvar in
    Pvar.is_self pvar
    && List.exists
         ~f:(fun {CapturedVar.name= captured; typ} ->
           Mangled.equal captured pvar_name && Typ.is_strong_pointer typ )
         attributes.ProcAttributes.captured


  (* The variable is captured in the block, contains self in the name, is not self, and it's strong. *)
  let is_captured_strong_self attributes pvar =
    (not (Pvar.is_self pvar))
    && List.exists
         ~f:(fun {CapturedVar.name= captured; typ} ->
           Typ.is_strong_pointer typ
           && Mangled.equal captured (Pvar.get_name pvar)
           && String.is_suffix ~suffix:"self" (String.lowercase (Mangled.to_string captured)) )
         attributes.ProcAttributes.captured


  let is_captured_weak_self attributes pvar =
    List.exists
      ~f:(fun {CapturedVar.name= captured; typ} ->
        Mangled.equal captured (Pvar.get_name pvar)
        && String.is_substring ~substring:"self" (String.lowercase (Mangled.to_string captured))
        && Typ.is_weak_pointer typ )
      attributes.ProcAttributes.captured


  let find_strong_var (domain : Domain.t) id =
    match Vars.find_opt id domain.vars with
    | Some elem when StrongEqualToWeakCapturedVars.mem elem.pvar domain.strongVars ->
        Some (elem, elem.pvar, StrongEqualToWeakCapturedVars.find elem.pvar domain.strongVars)
    | _ ->
        None


  let exec_null_check_id (astate : Domain.t) id =
    match find_strong_var astate id with
    | Some (elem, pvar, strongVarElem) ->
        let strongVars =
          StrongEqualToWeakCapturedVars.add pvar
            {strongVarElem with checked= true}
            astate.strongVars
        in
        let vars =
          (* We may have added UNCHECKED_STRONG_SELF in the previous Load instr,
             but this occurrence is not a bug, since we are checking right here! *)
          Vars.add id {elem with kind= CHECKED_STRONG_SELF} astate.vars
        in
        {Domain.vars; strongVars}
    | None ->
        astate


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


  let report_unchecked_strongself_issues proc_desc err_log (domain : Domain.t) var_use var =
    match find_strong_var domain var with
    | Some ({DomainData.pvar; loc; kind}, _, strongVarElem)
      when DomainData.is_unchecked_strong_self kind && not strongVarElem.reported ->
        let message =
          F.asprintf
            "The variable `%a`, equal to a weak pointer to `self`, is %s without a check for null \
             at %a. This could cause a crash or unexpected behavior."
            (Pvar.pp Pp.text) pvar var_use Location.pp loc
        in
        let ltr = make_trace_unchecked_strongself domain in
        Reporting.log_issue proc_desc err_log ~ltr ~loc SelfInBlock
          IssueType.strong_self_not_checked message ;
        let strongVars =
          StrongEqualToWeakCapturedVars.add pvar
            {strongVarElem with reported= true}
            domain.strongVars
        in
        {domain with strongVars}
    | _ ->
        domain


  let report_unchecked_strongself_issues_on_exps proc_desc err_log (domain : Domain.t)
      (instr : Sil.instr) =
    let report_unchecked_strongself_issues_on_exp strongVars (exp : Exp.t) =
      match exp with
      | Lfield (Var var, _, _) ->
          report_unchecked_strongself_issues proc_desc err_log domain "dereferenced" var
      | _ ->
          strongVars
    in
    let exps = Sil.exps_of_instr instr in
    List.fold ~f:report_unchecked_strongself_issues_on_exp exps ~init:domain


  (* The translation of closures includes a load instruction for the captured variable,
     then we add that corresponding id to the closure. This doesn't correspond to an
     actual "use" of the captured variable in the source program though, and causes false
     positives. Here we remove the ids from the domain when that id is being added to a closure. *)
  let remove_ids_in_closures_from_domain (domain : Domain.t) (instr : Sil.instr) =
    let remove_id_in_closures_from_domain vars ((exp : Exp.t), _, _, _) =
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


  let is_objc_instance attributes_opt =
    match attributes_opt with
    | Some proc_attrs -> (
      match proc_attrs.ProcAttributes.clang_method_kind with
      | ClangMethodKind.OBJC_INSTANCE ->
          true
      | _ ->
          false )
    | None ->
        false


  let get_annotations attributes_opt =
    match attributes_opt with
    | Some proc_attrs ->
        Some proc_attrs.ProcAttributes.method_annotation.params
    | None ->
        None


  let report_unchecked_strongself_issues_on_args proc_desc err_log (domain : Domain.t) pname args =
    let report_issue var =
      report_unchecked_strongself_issues proc_desc err_log domain
        (F.sprintf "passed to `%s`" (Procname.to_simplified_string pname))
        var
    in
    let rec report_on_non_nullable_arg ?annotations domain args =
      match (annotations, args) with
      | Some (annot :: annot_rest), (arg, _) :: rest ->
          let domain =
            match arg with
            | Exp.Var var when not (Annotations.ia_is_nullable annot) ->
                report_issue var
            | _ ->
                domain
          in
          report_on_non_nullable_arg ~annotations:annot_rest domain rest
      | None, (arg, _) :: rest ->
          let domain = match arg with Exp.Var var -> report_issue var | _ -> domain in
          report_on_non_nullable_arg domain rest
      | _ ->
          domain
    in
    let attributes_opt = AnalysisCallbacks.proc_resolve_attributes pname in
    let annotations = get_annotations attributes_opt in
    let args =
      if is_objc_instance attributes_opt then match args with _ :: rest -> rest | [] -> []
      else args
    in
    let annotations =
      if is_objc_instance attributes_opt then
        match annotations with Some (_ :: rest) -> Some rest | _ -> annotations
      else annotations
    in
    report_on_non_nullable_arg ?annotations domain args


  let exec_instr (astate : Domain.t) {IntraproceduralAnalysis.proc_desc; err_log} _cfg_node
      (instr : Sil.instr) =
    let attributes = Procdesc.get_attributes proc_desc in
    let astate = report_unchecked_strongself_issues_on_exps proc_desc err_log astate instr in
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
            then
              StrongEqualToWeakCapturedVars.add pvar
                {checked= false; loc; reported= false}
                astate.strongVars
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
        report_unchecked_strongself_issues_on_args proc_desc err_log astate callee_pn args
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


let report_mix_self_weakself_issues proc_desc err_log domain (weakSelf : DomainData.t)
    (self : DomainData.t) =
  let message =
    F.asprintf
      "This block uses both `%a` (%a) and `%a` (%a). This could lead to retain cycles or \
       unexpected behavior."
      (Pvar.pp Pp.text) weakSelf.pvar Location.pp weakSelf.loc (Pvar.pp Pp.text) self.pvar
      Location.pp self.loc
  in
  let ltr = make_trace_use_self_weakself domain in
  Reporting.log_issue proc_desc err_log ~ltr ~loc:self.loc SelfInBlock IssueType.mixed_self_weakself
    message


let report_weakself_in_no_escape_block_issues proc_desc err_log domain (weakSelf : DomainData.t)
    procname reported_weak_self_in_noescape_block =
  if not (Pvar.Set.mem weakSelf.pvar reported_weak_self_in_noescape_block) then (
    let reported_weak_self_in_noescape_block =
      Pvar.Set.add weakSelf.pvar reported_weak_self_in_noescape_block
    in
    let message =
      F.asprintf
        "This block uses `%a` at %a. This is probably not needed since the block is passed to the \
         method `%s` in a position annotated with NS_NOESCAPE. Use `self` instead."
        (Pvar.pp Pp.text) weakSelf.pvar Location.pp weakSelf.loc
        (Procname.to_simplified_string procname)
    in
    let ltr = make_trace_use_self_weakself domain in
    Reporting.log_issue proc_desc err_log ~ltr ~loc:weakSelf.loc SelfInBlock
      IssueType.weak_self_in_noescape_block message ;
    reported_weak_self_in_noescape_block )
  else reported_weak_self_in_noescape_block


let report_weakself_multiple_issue proc_desc err_log domain (weakSelf1 : DomainData.t)
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
  Reporting.log_issue proc_desc err_log ~ltr ~loc:weakSelf1.loc SelfInBlock
    IssueType.multiple_weakself message


let report_captured_strongself_issue proc_desc err_log domain (capturedStrongSelf : DomainData.t)
    report_captured_strongself =
  let attributes = Procdesc.get_attributes proc_desc in
  if
    Option.is_none attributes.ProcAttributes.passed_as_noescape_block_to
    && not (Pvar.Set.mem capturedStrongSelf.pvar report_captured_strongself)
  then (
    let report_captured_strongself =
      Pvar.Set.add capturedStrongSelf.pvar report_captured_strongself
    in
    let message =
      F.asprintf
        "The variable `%a`, used at `%a`, is a strong pointer to `self` captured in this block. \
         This could lead to retain cycles or unexpected behavior since to avoid retain cycles one \
         usually uses a local strong pointer or a captured weak pointer instead."
        (Pvar.pp Pp.text) capturedStrongSelf.pvar Location.pp capturedStrongSelf.loc
    in
    let ltr = make_trace_captured_strong_self domain in
    Reporting.log_issue proc_desc err_log ~ltr ~loc:capturedStrongSelf.loc SelfInBlock
      IssueType.captured_strong_self message ;
    report_captured_strongself )
  else report_captured_strongself


let report_issues proc_desc err_log domain =
  let process_domain_item (result : report_issues_result) (_, (domain_data : DomainData.t)) =
    match domain_data.kind with
    | DomainData.CAPTURED_STRONG_SELF ->
        let reported_captured_strong_self =
          report_captured_strongself_issue proc_desc err_log domain domain_data
            result.reported_captured_strong_self
        in
        {result with reported_captured_strong_self}
    | DomainData.WEAK_SELF ->
        let reported_weak_self_in_noescape_block =
          let attributes = Procdesc.get_attributes proc_desc in
          match attributes.ProcAttributes.passed_as_noescape_block_to with
          | Some procname ->
              report_weakself_in_no_escape_block_issues proc_desc err_log domain domain_data
                procname result.reported_weak_self_in_noescape_block
          | None ->
              result.reported_weak_self_in_noescape_block
        in
        { result with
          reported_weak_self_in_noescape_block
        ; weakSelfList= domain_data :: result.weakSelfList }
    | DomainData.SELF ->
        {result with selfList= domain_data :: result.selfList}
    | _ ->
        result
  in
  let report_issues_result_empty =
    { reported_captured_strong_self= Pvar.Set.empty
    ; reported_weak_self_in_noescape_block= Pvar.Set.empty
    ; selfList= []
    ; weakSelfList= [] }
  in
  let domain_bindings =
    Vars.bindings domain
    |> List.sort ~compare:(fun (_, {DomainData.loc= loc1}) (_, {DomainData.loc= loc2}) ->
           Location.compare loc1 loc2 )
  in
  let {weakSelfList; selfList} =
    List.fold_left ~f:process_domain_item ~init:report_issues_result_empty domain_bindings
  in
  let weakSelfList = List.rev weakSelfList in
  let selfList = List.rev selfList in
  ( match (weakSelfList, selfList) with
  | weakSelf :: _, self :: _ ->
      report_mix_self_weakself_issues proc_desc err_log domain weakSelf self
  | _ ->
      () ) ;
  match weakSelfList with
  | weakSelf1 :: weakSelf2 :: _ ->
      report_weakself_multiple_issue proc_desc err_log domain weakSelf1 weakSelf2
  | _ ->
      ()


module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

let checker ({IntraproceduralAnalysis.proc_desc; err_log} as analysis_data) =
  let initial = {Domain.vars= Vars.empty; strongVars= StrongEqualToWeakCapturedVars.empty} in
  let procname = Procdesc.get_proc_name proc_desc in
  if Procname.is_objc_block procname then
    match Analyzer.compute_post analysis_data ~initial proc_desc with
    | Some domain ->
        report_issues proc_desc err_log domain.vars
    | None ->
        ()
