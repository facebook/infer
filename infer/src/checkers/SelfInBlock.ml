(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

(* This will be looking for variables such as "strongSelf" or "weakSelf" *)
let contains_self pvar =
  let lowercase_name pvar = String.lowercase (Mangled.to_string (Pvar.get_name pvar)) in
  String.is_substring ~substring:(Mangled.to_string Mangled.self) (lowercase_name pvar)


module DomainData = struct
  type self_pointer_kind =
    | CAPTURED_STRONG_SELF
    | CHECKED_STRONG_SELF
    | SELF
    | UNCHECKED_STRONG_SELF
    | WEAK_SELF
    | CXX_REF of Location.t
    | INTERNAL_POINTER of Typ.t * Location.t
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
      | CXX_REF _ ->
          "CXX_REF"
      | INTERNAL_POINTER _ ->
          "INTERNAL POINTER"
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


  type t =
    { pvar: Pvar.t
    ; typ: Typ.t [@compare.ignore]
    ; loc: Location.t
    ; kind: self_pointer_kind
    ; is_implicit: bool }
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
  type t = {checked: bool; loc: Location.t; reported: bool} [@@deriving compare]

  let pp fmt {checked; reported; loc} =
    let s = match checked with true -> "Checked" | false -> "NotChecked" in
    let s' = match reported with true -> "Reported" | false -> "NotReported" in
    F.fprintf fmt "%s, %s, %a" s s' Location.pp loc


  let join {checked= elem1; loc= loc1; reported= reported1}
      {checked= elem2; loc= loc2; reported= reported2} =
    let loc, reported =
      match (elem1, elem2) with true, false -> (loc2, reported2) | _ -> (loc1, reported1)
    in
    {checked= elem1 && elem2; loc; reported}


  let widen ~prev ~next ~num_iters:_ = join prev next

  let leq ~lhs:{checked= lhs} ~rhs:{checked= rhs} = AbstractDomain.BooleanAnd.leq ~lhs ~rhs
end

module StrongEqualToWeakCapturedVars = struct
  include AbstractDomain.Map (PPPVar) (CheckedForNull)

  let compare = compare CheckedForNull.compare
end

module Vars = struct
  include AbstractDomain.Map (Ident) (DomainData)

  let compare = compare DomainData.compare
end

module Mem = struct
  type t = {vars: Vars.t; strongVars: StrongEqualToWeakCapturedVars.t} [@@deriving compare]

  let pp fmt {vars; strongVars} =
    F.fprintf fmt "%a@.%a" Vars.pp vars StrongEqualToWeakCapturedVars.pp strongVars


  let find_strong_var id (astate : t) =
    match Vars.find_opt id astate.vars with
    | Some elem when StrongEqualToWeakCapturedVars.mem elem.pvar astate.strongVars ->
        Some (elem, StrongEqualToWeakCapturedVars.find elem.pvar astate.strongVars)
    | _ ->
        None


  let exec_null_check_id id (astate : t) =
    match find_strong_var id astate with
    | Some (elem, strongVarElem) ->
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
        {vars; strongVars}
    | None ->
        astate


  let make_trace_unchecked_strongself (astate : t) =
    let trace_elems_strongVars =
      StrongEqualToWeakCapturedVars.fold
        (fun pvar {loc} trace_elems ->
          let trace_elem_desc = F.asprintf "%a assigned here" (Pvar.pp Pp.text) pvar in
          let trace_elem = Errlog.make_trace_element 0 loc trace_elem_desc [] in
          trace_elem :: trace_elems )
        astate.strongVars []
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
        astate.vars []
    in
    let trace_elems = List.append trace_elems_strongVars trace_elems_vars in
    List.sort trace_elems ~compare:(fun {Errlog.lt_loc= loc1} {Errlog.lt_loc= loc2} ->
        Location.compare loc1 loc2 )


  let report_unchecked_strongself_issues proc_desc err_log var_use var (astate : t) =
    match find_strong_var var astate with
    | Some ({DomainData.pvar; loc; kind}, strongVarElem)
      when DomainData.is_unchecked_strong_self kind
           && (not strongVarElem.reported) && contains_self pvar ->
        let message =
          F.asprintf
            "The variable `%a`, equal to a weak pointer to `self`, is %s without a check for null \
             at %a. This could cause a crash or unexpected behavior."
            (Pvar.pp Pp.text) pvar var_use Location.pp loc
        in
        let ltr = make_trace_unchecked_strongself astate in
        let attributes = Procdesc.get_attributes proc_desc in
        let return_typ = attributes.ProcAttributes.ret_type in
        let autofix =
          match StrongEqualToWeakCapturedVars.find_opt pvar astate.strongVars with
          | Some {loc} when Typ.is_void return_typ ->
              let strong_self_line = loc.line in
              let replacement =
                F.asprintf "\n if (!%s) { return; }" (Mangled.to_string (Pvar.get_name pvar))
              in
              Some
                { Jsonbug_t.original= None
                ; replacement= None
                ; additional=
                    Some
                      [{Jsonbug_t.line= strong_self_line + 1; column= 1; original= ""; replacement}]
                }
          | _ ->
              None
        in
        Reporting.log_issue proc_desc err_log ~ltr ~loc SelfInBlock ?autofix
          IssueType.strong_self_not_checked message ;
        let strongVars =
          StrongEqualToWeakCapturedVars.add pvar
            {strongVarElem with reported= true}
            astate.strongVars
        in
        {astate with strongVars}
    | _ ->
        astate


  (* The translation of closures includes a load instruction for the captured variable,
     then we add that corresponding id to the closure. This doesn't correspond to an
     actual "use" of the captured variable in the source program though, and causes false
     positives. Here we remove the ids from the domain when that id is being added to a closure. *)
  let remove_ids_in_closures_from_domain (instr : Sil.instr) (astate : t) =
    let remove_id_in_closures_from_domain vars ((exp : Exp.t), _) =
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
    let vars = List.fold ~init:astate.vars ~f:do_exp exps in
    {astate with vars}


  let pvar_same_name a b = Mangled.equal (Pvar.get_name a) (Pvar.get_name b)

  let is_captured_self attributes pvar =
    Pvar.is_self pvar
    && List.exists
         ~f:(fun {CapturedVar.pvar= captured; typ} ->
           pvar_same_name captured pvar && Typ.is_strong_pointer typ )
         attributes.ProcAttributes.captured


  (* The variable is captured in the block, contains self in the name, is not self, and it's strong. *)
  let is_captured_strong_self attributes pvar =
    (not (Pvar.is_self pvar))
    && List.exists
         ~f:(fun {CapturedVar.pvar= captured; typ} ->
           Typ.is_strong_pointer typ && pvar_same_name captured pvar && contains_self pvar )
         attributes.ProcAttributes.captured


  let is_captured_weak_self attributes pvar =
    List.exists
      ~f:(fun {CapturedVar.pvar= captured; typ} ->
        pvar_same_name captured pvar && contains_self pvar && Typ.is_weak_pointer typ )
      attributes.ProcAttributes.captured


  let get_captured_var_type typ capture_mode =
    (* In the frontend, if the variable is captured by reference, which is the case if it's
        declared with the __block attribute, we add an extra reference to the type, so we need to
        remove it here to check the real type. *)
    if CapturedVar.is_captured_by_ref capture_mode then
      match typ with
      | {Typ.desc= Typ.Tptr (t, _)} ->
          t
      | _ ->
          Logging.die InternalError "Not a possible case because of frontend constraints."
    else typ


  let is_ref typ capture_mode =
    let typ = get_captured_var_type typ capture_mode in
    Typ.is_reference typ


  let get_captured_of_type_loc is_type attributes pvar =
    List.find_map attributes.ProcAttributes.captured
      ~f:(fun {CapturedVar.pvar= captured; typ; capture_mode; captured_from} ->
        match (captured_from : CapturedVar.captured_info option) with
        | Some {loc} when pvar_same_name captured pvar && is_type typ capture_mode ->
            Some loc
        | _ ->
            None )


  let get_captured_ref_loc attributes pvar = get_captured_of_type_loc is_ref attributes pvar

  let get_captured_internal_pointer_loc attributes pvar =
    List.find_map attributes.ProcAttributes.captured
      ~f:(fun {CapturedVar.pvar= captured; captured_from; context_info} ->
        match ((captured_from : CapturedVar.captured_info option), context_info) with
        | Some {loc}, Some context_info when pvar_same_name captured pvar -> (
          match context_info.CapturedVar.is_internal_pointer_of with
          | Some typ ->
              Some (typ, loc)
          | None ->
              None )
        | _ ->
            None )


  let load attributes id pvar loc typ astate =
    let vars =
      if is_captured_self attributes pvar then
        Vars.add id {pvar; typ; loc; kind= SELF; is_implicit= false} astate.vars
      else if is_captured_strong_self attributes pvar then
        Vars.add id {pvar; typ; loc; kind= CAPTURED_STRONG_SELF; is_implicit= false} astate.vars
      else if is_captured_weak_self attributes pvar then
        Vars.add id {pvar; typ; loc; kind= WEAK_SELF; is_implicit= false} astate.vars
      else
        match get_captured_ref_loc attributes pvar with
        | Some captured_definition_loc ->
            Vars.add id
              {pvar; typ; loc; kind= CXX_REF captured_definition_loc; is_implicit= false}
              astate.vars
        | None -> (
          match get_captured_internal_pointer_loc attributes pvar with
          | Some (typ, captured_definition_loc) ->
              Vars.add id
                { pvar
                ; typ
                ; loc
                ; kind= INTERNAL_POINTER (typ, captured_definition_loc)
                ; is_implicit= false }
                astate.vars
          | _ -> (
            try
              let isChecked = StrongEqualToWeakCapturedVars.find pvar astate.strongVars in
              if not isChecked.checked then
                Vars.add id
                  {pvar; typ; loc; kind= UNCHECKED_STRONG_SELF; is_implicit= false}
                  astate.vars
              else astate.vars
            with Caml.Not_found -> astate.vars ) )
    in
    {astate with vars}


  let process_exp exp astate =
    match exp with
    | Exp.Lfield ({exp= Exp.Var id; is_implicit}, _, _) -> (
      match Vars.find_opt id astate.vars with
      | Some ivars_data when is_implicit ->
          let vars = Vars.add id {ivars_data with is_implicit} astate.vars in
          {astate with vars}
      | _ ->
          astate )
    | _ ->
        astate


  let store attributes pvar id pvar_typ loc astate =
    let strongVars =
      try
        let {DomainData.pvar= binding_for_id} = Vars.find id astate.vars in
        let strong_pvar_opt =
          StrongEqualToWeakCapturedVars.find_opt binding_for_id astate.strongVars
        in
        if
          (is_captured_weak_self attributes binding_for_id && Typ.is_strong_pointer pvar_typ)
          || Option.is_some strong_pvar_opt
        then
          StrongEqualToWeakCapturedVars.add pvar
            {checked= false; loc; reported= false}
            astate.strongVars
        else astate.strongVars
      with Caml.Not_found -> astate.strongVars
    in
    {astate with strongVars}
end

module Domain = struct
  include AbstractDomain.FiniteSet (Mem)

  let exec_null_check_id id astate = map (Mem.exec_null_check_id id) astate

  let report_unchecked_strongself_issues proc_desc err_log var_use var astate =
    map (Mem.report_unchecked_strongself_issues proc_desc err_log var_use var) astate


  let remove_ids_in_closures_from_domain instr astate =
    map (Mem.remove_ids_in_closures_from_domain instr) astate


  let load attributes id pvar loc typ astate = map (Mem.load attributes id pvar loc typ) astate

  let process_exp exp astate = map (Mem.process_exp exp) astate

  let store attributes pvar id pvar_typ loc astate =
    map (Mem.store attributes pvar id pvar_typ loc) astate
end

type report_issues_result = {selfList: DomainData.t list; weakSelfList: DomainData.t list}

module TransferFunctions = struct
  module Domain = Domain
  module CFG = ProcCfg.Normal

  type analysis_data = IntraproceduralAnalysis.t

  let pp_session_name _node fmt = F.pp_print_string fmt "SelfCapturedInBlock"

  let report_unchecked_strongself_issues_on_exps proc_desc err_log (domain : Domain.t)
      (instr : Sil.instr) =
    let report_unchecked_strongself_issues_on_exp strongVars (exp : Exp.t) =
      match exp with
      | Lfield ({exp= Var var}, _, _) ->
          Domain.report_unchecked_strongself_issues proc_desc err_log "dereferenced" var domain
      | _ ->
          strongVars
    in
    let exps = Sil.exps_of_instr instr in
    List.fold ~f:report_unchecked_strongself_issues_on_exp exps ~init:domain


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
        Some (List.map proc_attrs.ProcAttributes.formals ~f:trd3)
    | None ->
        None


  let report_unchecked_strongself_issues_on_args proc_desc err_log (domain : Domain.t) pname args =
    let report_issue var =
      Domain.report_unchecked_strongself_issues proc_desc err_log
        (F.sprintf "passed to `%s`" (Procname.to_simplified_string pname))
        var domain
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
    let attributes_opt = Attributes.load pname in
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


  let exec_instr (astate : Domain.t) {IntraproceduralAnalysis.proc_desc; err_log} _cfg_node _
      (instr : Sil.instr) =
    let attributes = Procdesc.get_attributes proc_desc in
    let astate = report_unchecked_strongself_issues_on_exps proc_desc err_log astate instr in
    let astate = Domain.remove_ids_in_closures_from_domain instr astate in
    match instr with
    | Load {id; e= Lvar pvar; loc; typ} ->
        Domain.load attributes id pvar loc typ astate
    | Load {e} ->
        Domain.process_exp e astate
    | Store {e1= Lvar pvar; e2= Var id; typ= pvar_typ; loc} ->
        Domain.store attributes pvar id pvar_typ loc astate
    | Store {e1; e2} ->
        let astate = Domain.process_exp e1 astate in
        Domain.process_exp e2 astate
    | Prune (Var id, _, _, _) ->
        Domain.exec_null_check_id id astate
    (* If (strongSelf != nil) or equivalent else branch *)
    | Prune (BinOp (Binop.Ne, Var id, e), _, _, _)
    (* If (!(strongSelf == nil)) or equivalent else branch *)
    | Prune (UnOp (LNot, BinOp (Binop.Eq, Var id, e), _), _, _, _) ->
        if Exp.is_null_literal e then Domain.exec_null_check_id id astate else astate
    | Call (_, Exp.Const (Const.Cfun callee_pn), args, _, _) ->
        let astate =
          List.fold ~init:astate ~f:(fun astate (exp, _) -> Domain.process_exp exp astate) args
        in
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


let make_trace_captured domain var =
  let trace_elems =
    Vars.fold
      (fun _ {pvar; loc; kind} trace_elems ->
        match kind with
        | (CAPTURED_STRONG_SELF | CXX_REF _ | SELF | INTERNAL_POINTER _) when Pvar.equal pvar var ->
            let trace_elem_desc = F.asprintf "Using captured %a" (Pvar.pp Pp.text) pvar in
            let trace_elem = Errlog.make_trace_element 0 loc trace_elem_desc [] in
            trace_elem :: trace_elems
        | _ ->
            trace_elems )
      domain []
  in
  List.sort trace_elems ~compare:(fun {Errlog.lt_loc= loc1} {Errlog.lt_loc= loc2} ->
      Location.compare loc1 loc2 )


let find_strong_self domain =
  Vars.fold
    (fun _ {pvar; kind; loc} pvar_opt ->
      match kind with
      | (UNCHECKED_STRONG_SELF | CHECKED_STRONG_SELF) when contains_self pvar ->
          Some (pvar, loc)
      | _ ->
          pvar_opt )
    domain None


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
  let to_string pvar = Mangled.to_string (Pvar.get_name pvar) in
  let strongSelf_opt = find_strong_self domain in
  let autofix =
    Option.bind
      ~f:(fun (strongSelf, strongSelfLoc) ->
        if
          strongSelfLoc.Location.line < self.loc.line
          && Pvar.is_syntactic self.pvar
          && Option.is_none self.loc.Location.macro_file_opt
        then
          let original, replacement =
            if self.is_implicit then ("", F.asprintf "%s->" (to_string strongSelf))
            else (to_string self.DomainData.pvar, to_string strongSelf)
          in
          Some {Jsonbug_j.original= Some original; replacement= Some replacement; additional= None}
        else None )
      strongSelf_opt
  in
  Reporting.log_issue proc_desc err_log ~ltr ~loc:self.loc ?autofix SelfInBlock
    IssueType.mixed_self_weakself message


let report_weakself_in_no_escape_block_issues proc_desc err_log domain (weakSelf : DomainData.t)
    procname =
  let message =
    F.asprintf
      "This block uses `%a` at %a. This is probably not needed since the block is passed to the \
       method `%s` in a position annotated with NS_NOESCAPE."
      (Pvar.pp Pp.text) weakSelf.pvar Location.pp weakSelf.loc
      (Procname.to_simplified_string procname)
  in
  let suggestion = "Use `self` instead." in
  let ltr = make_trace_use_self_weakself domain in
  Reporting.log_issue ~suggestion proc_desc err_log ~ltr ~loc:weakSelf.loc SelfInBlock
    IssueType.weak_self_in_noescape_block message


let report_weakself_multiple_issue proc_desc err_log domain (weakSelf1 : DomainData.t)
    (weakSelf2 : DomainData.t) =
  let message =
    F.asprintf
      "This block uses the weak pointer `%a` more than once (%a) and (%a). This could lead to \
       unexpected behavior. Even if `%a` is not nil in the first use, it could be nil in the \
       following uses since the object that `%a` points to could be freed anytime."
      (Pvar.pp Pp.text) weakSelf1.pvar Location.pp weakSelf1.loc Location.pp weakSelf2.loc
      (Pvar.pp Pp.text) weakSelf1.pvar (Pvar.pp Pp.text) weakSelf1.pvar
  in
  let suggestion = "Assign it to a strong variable first." in
  let ltr = make_trace_use_self_weakself domain in
  Reporting.log_issue ~suggestion proc_desc err_log ~ltr ~loc:weakSelf1.loc SelfInBlock
    IssueType.multiple_weakself message


let report_captured_strongself_issue proc_desc err_log domain (capturedStrongSelf : DomainData.t) =
  let attributes = Procdesc.get_attributes proc_desc in
  let passed_as_noescape_block =
    Option.value_map
      ~f:(fun ({passed_as_noescape_block} : ProcAttributes.block_as_arg_attributes) ->
        passed_as_noescape_block )
      ~default:false attributes.block_as_arg_attributes
  in
  if not passed_as_noescape_block then
    let message =
      F.asprintf
        "The variable `%a`, used at `%a`, is a strong pointer to `self` captured in this block. \
         This could lead to retain cycles or unexpected behavior."
        (Pvar.pp Pp.text) capturedStrongSelf.pvar Location.pp capturedStrongSelf.loc
    in
    let suggestion = "Use a local strong pointer or a captured weak pointer instead." in
    let ltr = make_trace_captured domain capturedStrongSelf.pvar in
    Reporting.log_issue ~suggestion proc_desc err_log ~ltr ~loc:capturedStrongSelf.loc SelfInBlock
      IssueType.captured_strong_self message
  else ()


let report_self_in_block_passed_to_init_issue proc_desc err_log domain (capturedSelf : DomainData.t)
    =
  let attributes = Procdesc.get_attributes proc_desc in
  let passed_to_init =
    Option.bind
      ~f:(fun ({passed_to} : ProcAttributes.block_as_arg_attributes) ->
        if Procname.is_objc_init passed_to then Some passed_to else None )
      attributes.block_as_arg_attributes
  in
  match passed_to_init with
  | Some passed_to_init ->
      let message =
        F.asprintf
          "`self` is captured in the block at %a. The block is passed to the initializer `%a`. \
           This could lead to retain cycles or unexpected behavior."
          Location.pp capturedSelf.loc Procname.pp passed_to_init
      in
      let ltr = make_trace_captured domain capturedSelf.pvar in
      Reporting.log_issue proc_desc err_log ~ltr ~loc:capturedSelf.loc SelfInBlock
        IssueType.self_in_block_passed_to_init message
  | None ->
      ()


let noescaping_matcher = QualifiedCppName.Match.of_fuzzy_qual_names Config.noescaping_function_list

let should_ignore_cxx_captured attributes =
  match attributes.ProcAttributes.block_as_arg_attributes with
  | Some {passed_to; passed_as_noescape_block} ->
      passed_as_noescape_block
      || QualifiedCppName.Match.match_qualifiers noescaping_matcher
           (Procname.get_qualifiers passed_to)
  | None ->
      true


let init_trace_captured_var data captured_definition_loc =
  let trace_elem_desc =
    F.asprintf "Captured variable %a defined here" (Pvar.pp Pp.text) data.DomainData.pvar
  in
  Errlog.make_trace_element 0 captured_definition_loc trace_elem_desc []


let report_cxx_ref_captured_in_block proc_desc err_log domain (cxx_ref : DomainData.t)
    captured_definition_loc =
  let attributes = Procdesc.get_attributes proc_desc in
  if not (should_ignore_cxx_captured attributes) then
    let message =
      F.asprintf
        "The variable `%a` is a C++ reference and it's captured in the block. This can lead to \
         crashes."
        (Pvar.pp Pp.text) cxx_ref.pvar
    in
    let init_trace_elem = init_trace_captured_var cxx_ref captured_definition_loc in
    let ltr = init_trace_elem :: make_trace_captured domain cxx_ref.pvar in
    Reporting.log_issue proc_desc err_log ~ltr ~loc:cxx_ref.loc SelfInBlock
      IssueType.cxx_ref_captured_in_block message
  else ()


let std_string_matcher = QualifiedCppName.Match.of_fuzzy_qual_names ["std::basic_string"]

let is_std_string typ =
  match typ.Typ.desc with
  | Tstruct name ->
      let qual_name = Typ.Name.qual_name name in
      QualifiedCppName.Match.match_qualifiers std_string_matcher qual_name
  | _ ->
      false


let report_internal_pointer_captured_in_block proc_desc err_log domain (cxx_string : DomainData.t)
    typ_string captured_definition_loc =
  let attributes = Procdesc.get_attributes proc_desc in
  if not (should_ignore_cxx_captured attributes) then
    let message =
      F.asprintf
        "The local variable `%a` is a pointer internal to a `%s` object and it's captured in the \
         block. This can lead to crashes if the object is freed before the block's execution."
        (Pvar.pp Pp.text) cxx_string.pvar typ_string
    in
    let init_trace_elem = init_trace_captured_var cxx_string captured_definition_loc in
    let ltr = init_trace_elem :: make_trace_captured domain cxx_string.pvar in
    Reporting.log_issue proc_desc err_log ~ltr ~loc:cxx_string.loc SelfInBlock
      IssueType.cxx_string_captured_in_block message
  else ()


let report_issues proc_desc err_log domain =
  let process_domain_item (result : report_issues_result) (_, (domain_data : DomainData.t)) =
    match domain_data.kind with
    | DomainData.CAPTURED_STRONG_SELF ->
        report_captured_strongself_issue proc_desc err_log domain domain_data ;
        result
    | DomainData.CXX_REF captured_definition_loc ->
        report_cxx_ref_captured_in_block proc_desc err_log domain domain_data
          captured_definition_loc ;
        result
    | DomainData.INTERNAL_POINTER (typ, captured_definition_loc) when is_std_string typ ->
        report_internal_pointer_captured_in_block proc_desc err_log domain domain_data "std::string"
          captured_definition_loc ;
        result
    | DomainData.WEAK_SELF ->
        let _ =
          let attributes = Procdesc.get_attributes proc_desc in
          match attributes.ProcAttributes.block_as_arg_attributes with
          | Some {passed_to= procname; passed_as_noescape_block= true} ->
              report_weakself_in_no_escape_block_issues proc_desc err_log domain domain_data
                procname
          | _ ->
              ()
        in
        {result with weakSelfList= domain_data :: result.weakSelfList}
    | DomainData.SELF ->
        report_self_in_block_passed_to_init_issue proc_desc err_log domain domain_data ;
        {result with selfList= domain_data :: result.selfList}
    | _ ->
        result
  in
  let domain_bindings =
    Vars.bindings domain
    |> List.sort ~compare:(fun (_, {DomainData.loc= loc1}) (_, {DomainData.loc= loc2}) ->
           Location.compare loc1 loc2 )
  in
  let report_issues_result_empty = {selfList= []; weakSelfList= []} in
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
  let initial =
    Domain.singleton {Mem.vars= Vars.empty; strongVars= StrongEqualToWeakCapturedVars.empty}
  in
  let procname = Procdesc.get_proc_name proc_desc in
  if Procname.is_objc_block procname then
    match Analyzer.compute_post analysis_data ~initial proc_desc with
    | Some domain ->
        Domain.iter (fun {vars} -> report_issues proc_desc err_log vars) domain
    | None ->
        ()
