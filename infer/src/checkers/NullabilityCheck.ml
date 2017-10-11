(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module L = Logging
module MF = MarkupFormatter
module CallSites = AbstractDomain.FiniteSet (CallSite)
module Domain = AbstractDomain.Map (AccessPath) (CallSites)

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type extras = Specs.summary

  let is_instance_method callee_pname =
    if Typ.Procname.is_java callee_pname then not (Typ.Procname.java_is_static callee_pname)
    else
      Option.exists
        ~f:(fun attributes ->
          attributes.ProcAttributes.is_objc_instance_method
          || attributes.ProcAttributes.is_cpp_instance_method)
        (Specs.proc_resolve_attributes callee_pname)

  let report_nullable_dereference ap call_sites {ProcData.pdesc; extras} loc =
    let pname = Procdesc.get_proc_name pdesc in
    let annotation = Localise.nullable_annotation_name pname in
    let issue_kind = IssueType.nullable_dereference.unique_id in
    let call_site =
      try CallSites.min_elt call_sites
      with Not_found ->
        L.(die InternalError)
          "Expecting a least one element in the set of call sites when analyzing %a"
          Typ.Procname.pp pname
    in
    let message =
      Format.asprintf
        "Variable %a is indirectly annotated with %a (source %a) and is dereferenced without being checked for null"
        (MF.wrap_monospaced AccessPath.pp) ap MF.pp_monospaced annotation
        (MF.wrap_monospaced CallSite.pp) call_site
    in
    let exn = Exceptions.Checkers (issue_kind, Localise.verbatim_desc message) in
    let summary = extras in
    let trace =
      let with_origin_site =
        let callee_pname = CallSite.pname call_site in
        match Specs.proc_resolve_attributes callee_pname with
        | None
         -> []
        | Some attributes
         -> let description =
              Format.asprintf "definition of %s" (Typ.Procname.get_method callee_pname)
            in
            let trace_element =
              Errlog.make_trace_element 1 attributes.ProcAttributes.loc description []
            in
            [trace_element]
      in
      let with_assignment_site =
        let call_site_loc = CallSite.loc call_site in
        if Location.equal call_site_loc loc then with_origin_site
        else
          let trace_element =
            Errlog.make_trace_element 0 call_site_loc "assignment of the nullable value" []
          in
          trace_element :: with_origin_site
      in
      let dereference_site =
        let description = Format.asprintf "deference of %a" AccessPath.pp ap in
        Errlog.make_trace_element 0 loc description []
      in
      dereference_site :: with_assignment_site
    in
    Reporting.log_error summary ~loc ~ltr:trace exn

  let exec_instr (astate: Domain.astate) proc_data _ (instr: HilInstr.t) : Domain.astate =
    match instr with
    | Call (Some ret_var, Direct callee_pname, _, _, loc)
      when Annotations.pname_has_return_annot callee_pname
             ~attrs_of_pname:Specs.proc_resolve_attributes Annotations.ia_is_nullable
     -> let call_site = CallSite.make callee_pname loc in
        Domain.add (ret_var, []) (CallSites.singleton call_site) astate
    | Call (_, Direct callee_pname, (HilExp.AccessPath receiver) :: _, _, loc)
      when is_instance_method callee_pname -> (
      match Domain.find_opt receiver astate with
      | None
       -> astate
      | Some call_sites
       -> report_nullable_dereference receiver call_sites proc_data loc ;
          Domain.remove receiver astate )
    | Call (Some ret_var, _, _, _, _)
     -> Domain.remove (ret_var, []) astate
    | Assign (lhs, _, loc) when Domain.mem lhs astate
     -> report_nullable_dereference lhs (Domain.find lhs astate) proc_data loc ;
        Domain.remove lhs astate
    | Assign (lhs, HilExp.AccessPath rhs, _) when Domain.mem rhs astate
     -> Domain.add lhs (Domain.find rhs astate) astate
    | Assign (lhs, _, _)
     -> Domain.remove lhs astate
    | Assume (HilExp.AccessPath ap, _, _, _)
     -> Domain.remove ap astate
    | Assume (HilExp.BinaryOperator (Binop.Ne, HilExp.AccessPath ap, exp), _, _, _)
      when HilExp.is_null_literal exp
     -> Domain.remove ap astate
    | _
     -> astate
end

module Analyzer =
  AbstractInterpreter.Make (ProcCfg.Exceptional) (LowerHil.MakeDefault (TransferFunctions))

let checker {Callbacks.summary; proc_desc; tenv} =
  let initial = (Domain.empty, IdAccessPathMapDomain.empty) in
  let proc_data = ProcData.make proc_desc tenv summary in
  ignore (Analyzer.compute_post proc_data ~initial ~debug:false) ;
  summary
