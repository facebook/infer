(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module F = Format
module L = Logging
module MF = MarkupFormatter
module CallSites = AbstractDomain.FiniteSet (CallSite)
module NullableAP = AbstractDomain.Map (AccessPath) (CallSites)
module NullCheckedPname = AbstractDomain.InvertedSet (Typ.Procname)
module Domain = AbstractDomain.Pair (NullableAP) (NullCheckedPname)

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type extras = Specs.summary

  let is_instance_method callee_pname =
    if Typ.Procname.is_java callee_pname then not (Typ.Procname.java_is_static callee_pname)
    else
      Option.exists
        ~f:(fun attributes -> attributes.ProcAttributes.is_cpp_instance_method)
        (Specs.proc_resolve_attributes callee_pname)


  let is_blacklisted callee_pname =
    let blacklist = ["URLWithString:"]
    and simplified_callee_pname = Typ.Procname.to_simplified_string callee_pname in
    List.exists ~f:(String.equal simplified_callee_pname) blacklist


  let report_nullable_dereference ap call_sites {ProcData.pdesc; extras} loc =
    let pname = Procdesc.get_proc_name pdesc in
    let annotation = Localise.nullable_annotation_name pname in
    let issue_kind = IssueType.nullable_dereference.unique_id in
    let call_site =
      try CallSites.min_elt call_sites with Not_found ->
        L.(die InternalError)
          "Expecting a least one element in the set of call sites when analyzing %a"
          Typ.Procname.pp pname
    in
    let simplified_pname =
      Typ.Procname.to_simplified_string ~withclass:true (CallSite.pname call_site)
    in
    let is_direct_dereference =
      match ap with
      | (Var.LogicalVar _, _), _ ->
          true
      | (Var.ProgramVar pvar, _), _ ->
          Pvar.is_frontend_tmp pvar
    in
    let message =
      if is_direct_dereference then
        (* direct dereference without intermediate variable *)
        F.asprintf
          "The return value of %s is annotated with %a and is dereferenced without being checked for null at %a"
          (MF.monospaced_to_string simplified_pname)
          MF.pp_monospaced annotation Location.pp loc
      else
        (* dereference with intermediate variable *)
        F.asprintf
          "Variable %a is indirectly annotated with %a (source %a) and is dereferenced without being checked for null at %a"
          (MF.wrap_monospaced AccessPath.pp)
          ap MF.pp_monospaced annotation (MF.wrap_monospaced CallSite.pp) call_site Location.pp loc
    in
    let exn = Exceptions.Checkers (issue_kind, Localise.verbatim_desc message) in
    let summary = extras in
    let trace =
      let with_origin_site =
        let callee_pname = CallSite.pname call_site in
        match Specs.proc_resolve_attributes callee_pname with
        | None ->
            []
        | Some attributes ->
            let description =
              F.asprintf "definition of %s" (Typ.Procname.get_method callee_pname)
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
        let description =
          if is_direct_dereference then
            F.asprintf "dereferencing the return of %s" simplified_pname
          else F.asprintf "dereference of %a" AccessPath.pp ap
        in
        Errlog.make_trace_element 0 loc description []
      in
      dereference_site :: with_assignment_site
    in
    Reporting.log_error summary ~loc ~ltr:trace exn


  let add_nullable_ap ap call_sites (aps, pnames) = (NullableAP.add ap call_sites aps, pnames)

  let remove_nullable_ap ap (aps, pnames) = (NullableAP.remove ap aps, pnames)

  let find_nullable_ap ap (aps, _) = NullableAP.find ap aps

  let assume_pnames_notnull ap (aps, checked_pnames) : Domain.astate =
    let updated_pnames =
      try
        let call_sites = NullableAP.find ap aps in
        CallSites.fold
          (fun call_site s -> NullCheckedPname.add (CallSite.pname call_site) s)
          call_sites checked_pnames
      with Not_found -> checked_pnames
    in
    (NullableAP.remove ap aps, updated_pnames)


  let rec longest_nullable_prefix ap ((nulable_aps, _) as astate) =
    try Some (ap, NullableAP.find ap nulable_aps) with Not_found ->
      match ap with _, [] -> None | p -> longest_nullable_prefix (AccessPath.truncate p) astate


  let check_ap proc_data loc ap astate =
    match longest_nullable_prefix ap astate with
    | None ->
        astate
    | Some (nullable_ap, call_sites) ->
        report_nullable_dereference nullable_ap call_sites proc_data loc ;
        assume_pnames_notnull ap astate


  let rec check_nil_in_nsarray proc_data loc args astate =
    match args with
    | [] ->
        astate
    | [arg] when HilExp.is_null_literal arg ->
        astate
    | (HilExp.AccessPath ap) :: other_args ->
        check_nil_in_nsarray proc_data loc other_args (check_ap proc_data loc ap astate)
    | _ :: other_args ->
        check_nil_in_nsarray proc_data loc other_args astate


  let exec_instr ((_, checked_pnames) as astate) proc_data _ (instr: HilInstr.t) : Domain.astate =
    let is_pointer_assignment tenv lhs rhs =
      HilExp.is_null_literal rhs
      (* the rhs has type int when assigning the lhs to null *)
      || Option.equal Typ.equal (AccessPath.get_typ lhs tenv) (HilExp.get_typ tenv rhs)
      (* the lhs and rhs have the same type in the case of pointer assignment
         but the types are different when assigning the pointee *)
    in
    match instr with
    | Call (Some ret_var, Direct callee_pname, _, _, _)
      when NullCheckedPname.mem callee_pname checked_pnames ->
        (* Do not report nullable when the method has already been checked for null *)
        remove_nullable_ap (ret_var, []) astate
    | Call (_, Direct callee_pname, (HilExp.AccessPath receiver) :: _, _, _)
      when Models.is_check_not_null callee_pname ->
        assume_pnames_notnull receiver astate
    | Call (_, Direct callee_pname, _, _, _) when is_blacklisted callee_pname ->
        astate
    | Call (Some ret_var, Direct callee_pname, _, _, loc)
      when Annotations.pname_has_return_annot callee_pname
             ~attrs_of_pname:Specs.proc_resolve_attributes Annotations.ia_is_nullable ->
        let call_site = CallSite.make callee_pname loc in
        add_nullable_ap (ret_var, []) (CallSites.singleton call_site) astate
    | Call (_, Direct callee_pname, (HilExp.AccessPath receiver) :: _, _, loc)
      when is_instance_method callee_pname ->
        check_ap proc_data loc receiver astate
    | Call (_, Direct callee_pname, args, _, loc)
      when Typ.Procname.equal callee_pname BuiltinDecl.nsArray_arrayWithObjectsCount ->
        check_nil_in_nsarray proc_data loc args astate
    | Call (Some ret_var, _, _, _, _) ->
        remove_nullable_ap (ret_var, []) astate
    | Assign (lhs, rhs, loc)
      -> (
        Option.iter
          ~f:(fun (nullable_ap, call_sites) ->
            if not (is_pointer_assignment proc_data.ProcData.tenv nullable_ap rhs) then
              (* TODO (T22426288): Undertand why the pointer derference and the pointer
                 assignment have the same HIL representation *)
              report_nullable_dereference nullable_ap call_sites proc_data loc)
          (longest_nullable_prefix lhs astate) ;
        match rhs with
        | HilExp.AccessPath ap -> (
          try
            (* Add the lhs to the list of nullable values if the rhs is nullable *)
            add_nullable_ap lhs (find_nullable_ap ap astate) astate
          with Not_found ->
            (* Remove the lhs from the list of nullable values if the rhs is not nullable *)
            remove_nullable_ap lhs astate )
        | _ ->
            (* Remove the lhs from the list of nullable values if the rhs is not an access path *)
            remove_nullable_ap lhs astate )
    | Assume (HilExp.AccessPath ap, _, _, _) ->
        assume_pnames_notnull ap astate
    | Assume
        ( ( HilExp.BinaryOperator (Binop.Ne, HilExp.AccessPath ap, exp)
          | HilExp.BinaryOperator (Binop.Ne, exp, HilExp.AccessPath ap) )
        , _
        , _
        , _ )
    | Assume
        ( HilExp.UnaryOperator
            ( Unop.LNot
            , ( HilExp.BinaryOperator (Binop.Eq, HilExp.AccessPath ap, exp)
              | HilExp.BinaryOperator (Binop.Eq, exp, HilExp.AccessPath ap) )
            , _ )
        , _
        , _
        , _ ) ->
        if HilExp.is_null_literal exp then assume_pnames_notnull ap astate else astate
    | _ ->
        astate

end

module Analyzer = LowerHil.MakeAbstractInterpreter (ProcCfg.Exceptional) (TransferFunctions)

let checker {Callbacks.summary; proc_desc; tenv} =
  let initial = (NullableAP.empty, NullCheckedPname.empty) in
  let proc_data = ProcData.make proc_desc tenv summary in
  ignore (Analyzer.compute_post proc_data ~initial) ;
  summary

