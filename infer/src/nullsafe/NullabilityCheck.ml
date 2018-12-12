(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
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

  type extras = Summary.t

  let rec is_pointer_subtype tenv typ1 typ2 =
    match (typ1.Typ.desc, typ2.Typ.desc) with
    | Typ.Tptr (t1, _), Typ.Tptr (t2, _) -> (
      match (t1.Typ.desc, t2.Typ.desc) with
      | Typ.Tstruct n1, Typ.Tstruct n2 ->
          Subtype.is_known_subtype tenv n1 n2
      | _ ->
          Typ.equal t1 t2 || is_pointer_subtype tenv t1 t2 )
    | _ ->
        false


  let is_non_objc_instance_method callee_pname =
    match callee_pname with
    | Typ.Procname.Java java_pname ->
        not (Typ.Procname.Java.is_static java_pname)
    | _ ->
        Option.exists
          ~f:(fun attributes ->
            ClangMethodKind.equal attributes.ProcAttributes.clang_method_kind
              ClangMethodKind.CPP_INSTANCE )
          (Summary.proc_resolve_attributes callee_pname)


  let is_objc_instance_method callee_pname =
    Option.exists
      ~f:(fun attributes ->
        ClangMethodKind.equal attributes.ProcAttributes.clang_method_kind
          ClangMethodKind.OBJC_INSTANCE )
      (Summary.proc_resolve_attributes callee_pname)


  let is_blacklisted_method : Typ.Procname.t -> bool =
    let blacklist = ["URLWithString:"; "objectForKeyedSubscript:"] in
    fun proc_name ->
      let simplified_callee_pname = Typ.Procname.to_simplified_string proc_name in
      List.exists ~f:(String.equal simplified_callee_pname) blacklist


  let container_method_regex =
    Str.regexp @@ "^\\(NS.*_\\(arrayByAddingObject\\|arrayWithObjects\\|"
    ^ "dictionaryWithObjects\\|dictionaryWithObjectsAndKeys\\|initWithObjectsAndKeys\\|"
    ^ "addObject\\|insertObject\\|setObject\\|"
    ^ "stringWithUTF8String\\|stringWithString\\|initWithFormat\\|stringByAppendingString\\):\\|"
    ^ "std::basic_string\\).*"


  let is_objc_container_add_method proc_name =
    let callee_pname = Typ.Procname.to_string proc_name in
    Str.string_match container_method_regex callee_pname 0


  let is_conflicting_report summary report_location =
    if not Config.filtering then false
    else
      Errlog.fold
        (fun {Errlog.err_name; err_desc} {Errlog.loc} found_confict ->
          found_confict
          || IssueType.equal err_name IssueType.null_dereference
             && Location.equal loc report_location
             && Localise.error_desc_is_reportable_bucket err_desc )
        (Summary.get_err_log summary) false


  (* On Clang languages, the annotations like _Nullabe can be found on the declaration
     or on the implementation without the two being necessarily consistent.
     Here, we explicitely want to lookup the annotations locally: either form
     the implementation when defined locally, or from the included headers *)
  let lookup_local_attributes = function
    | Typ.Procname.Java _ as pname ->
        (* Looking up the attribute according to the classpath *)
        Summary.proc_resolve_attributes pname
    | pname ->
        (* Looking up the attributes locally, i.e. either from the file of from the includes *)
        Option.map ~f:Procdesc.get_attributes (Ondemand.get_proc_desc pname)


  let report_nullable_dereference ap call_sites {ProcData.pdesc; extras} loc =
    let summary = extras in
    if is_conflicting_report summary loc then ()
    else
      let pname = Procdesc.get_proc_name pdesc in
      let annotation = Localise.nullable_annotation_name pname in
      let call_site =
        try CallSites.min_elt call_sites with Caml.Not_found ->
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
            "The return value of %s is annotated with %a and is dereferenced without being \
             checked for null at %a"
            (MF.monospaced_to_string simplified_pname)
            MF.pp_monospaced annotation Location.pp loc
        else
          (* dereference with intermediate variable *)
          F.asprintf
            "Variable %a is indirectly annotated with %a (source %a) and is dereferenced without \
             being checked for null at %a"
            (MF.wrap_monospaced AccessPath.pp)
            ap MF.pp_monospaced annotation (MF.wrap_monospaced CallSite.pp) call_site Location.pp
            loc
      in
      let trace =
        let with_origin_site =
          let callee_pname = CallSite.pname call_site in
          match lookup_local_attributes callee_pname with
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
      Reporting.log_error summary ~loc ~ltr:trace IssueType.nullable_dereference message


  let add_nullable_ap ap call_sites (aps, pnames) = (NullableAP.add ap call_sites aps, pnames)

  let remove_nullable_ap ap (aps, pnames) = (NullableAP.remove ap aps, pnames)

  let find_nullable_ap ap (aps, _) = NullableAP.find ap aps

  let assume_pnames_notnull ap (aps, checked_pnames) : Domain.t =
    let remove_call_sites ap aps =
      let add_diff (to_remove : CallSites.t) ap call_sites map =
        let remaining_call_sites = CallSites.diff call_sites to_remove in
        if CallSites.is_empty remaining_call_sites then map
        else NullableAP.add ap remaining_call_sites map
      in
      match NullableAP.find_opt ap aps with
      | None ->
          aps
      | Some call_sites ->
          let updated_aps = NullableAP.fold (add_diff call_sites) aps NullableAP.empty in
          updated_aps
    in
    let updated_pnames =
      try
        let call_sites = NullableAP.find ap aps in
        CallSites.fold
          (fun call_site s -> NullCheckedPname.add (CallSite.pname call_site) s)
          call_sites checked_pnames
      with Caml.Not_found -> checked_pnames
    in
    (remove_call_sites ap aps, updated_pnames)


  let rec longest_nullable_prefix ap ((nullable_aps, _) as astate) =
    try Some (ap, NullableAP.find ap nullable_aps) with Caml.Not_found -> (
      match ap with
      | _, [] ->
          None
      | p ->
          longest_nullable_prefix (fst (AccessPath.truncate p)) astate )


  let check_ap proc_data loc ap astate =
    match longest_nullable_prefix ap astate with
    | None ->
        astate
    | Some (nullable_ap, call_sites) ->
        report_nullable_dereference nullable_ap call_sites proc_data loc ;
        assume_pnames_notnull ap astate


  let rec check_nil_in_objc_container proc_data loc args astate =
    match args with
    | [] ->
        astate
    | [arg] when HilExp.is_null_literal arg ->
        astate
    | HilExp.AccessExpression access_expr :: other_args ->
        let ap = HilExp.AccessExpression.to_access_path access_expr in
        check_nil_in_objc_container proc_data loc other_args (check_ap proc_data loc ap astate)
    | _ :: other_args ->
        check_nil_in_objc_container proc_data loc other_args astate


  let exec_instr ((_, checked_pnames) as astate) proc_data _ (instr : HilInstr.t) : Domain.t =
    let is_pointer_assignment tenv lhs rhs =
      (* the rhs has type int when assigning the lhs to null *)
      if HilExp.is_null_literal rhs then true
        (* the lhs and rhs have the same type in the case of pointer assignment
         but the types are different when assigning the pointee *)
      else
        match (AccessPath.get_typ lhs tenv, HilExp.get_typ tenv rhs) with
        (* defensive assumption when the types are not known *)
        | None, _ | _, None ->
            true
        (* the rhs can be a subtype of the lhs *)
        | Some lhs_typ, Some rhs_typ ->
            is_pointer_subtype tenv rhs_typ lhs_typ
    in
    match instr with
    | Call (ret_var, Direct callee_pname, _, _, _)
      when NullCheckedPname.mem callee_pname checked_pnames ->
        (* Do not report nullable when the method has already been checked for null *)
        remove_nullable_ap (ret_var, []) astate
    | Call (_, Direct callee_pname, HilExp.AccessExpression receiver :: _, _, _)
      when Models.is_check_not_null callee_pname ->
        assume_pnames_notnull (HilExp.AccessExpression.to_access_path receiver) astate
    | Call (_, Direct callee_pname, _, _, _) when is_blacklisted_method callee_pname ->
        astate
    | Call (ret_var, Direct callee_pname, _, _, loc)
      when Annotations.pname_has_return_annot callee_pname ~attrs_of_pname:lookup_local_attributes
             Annotations.ia_is_nullable ->
        let call_site = CallSite.make callee_pname loc in
        add_nullable_ap (ret_var, []) (CallSites.singleton call_site) astate
    | Call (_, Direct callee_pname, args, _, loc) when is_objc_container_add_method callee_pname ->
        check_nil_in_objc_container proc_data loc args astate
    | Call (_, Direct callee_pname, HilExp.AccessExpression receiver :: _, _, loc)
      when is_non_objc_instance_method callee_pname ->
        check_ap proc_data loc (HilExp.AccessExpression.to_access_path receiver) astate
    | Call
        ( ((_, ret_typ) as ret_var)
        , Direct callee_pname
        , HilExp.AccessExpression receiver :: _
        , _
        , _ )
      when Typ.is_pointer ret_typ && is_objc_instance_method callee_pname -> (
      match longest_nullable_prefix (HilExp.AccessExpression.to_access_path receiver) astate with
      | None ->
          astate
      | Some (_, call_sites) ->
          (* Objective C method will return nil when called on a nil receiver *)
          add_nullable_ap (ret_var, []) call_sites astate )
    | Call (ret_var, _, _, _, _) ->
        remove_nullable_ap (ret_var, []) astate
    | Assign (lhs_access_expr, rhs, loc) -> (
        let lhs = HilExp.AccessExpression.to_access_path lhs_access_expr in
        Option.iter
          ~f:(fun (nullable_ap, call_sites) ->
            if not (is_pointer_assignment proc_data.ProcData.tenv nullable_ap rhs) then
              (* TODO (T22426288): Undertand why the pointer derference and the pointer
                 assignment have the same HIL representation *)
              report_nullable_dereference nullable_ap call_sites proc_data loc )
          (longest_nullable_prefix lhs astate) ;
        match rhs with
        | HilExp.AccessExpression access_expr -> (
          try
            (* Add the lhs to the list of nullable values if the rhs is nullable *)
            let ap = HilExp.AccessExpression.to_access_path access_expr in
            add_nullable_ap lhs (find_nullable_ap ap astate) astate
          with Caml.Not_found ->
            (* Remove the lhs from the list of nullable values if the rhs is not nullable *)
            remove_nullable_ap lhs astate )
        | _ ->
            (* Remove the lhs from the list of nullable values if the rhs is not an access path *)
            remove_nullable_ap lhs astate )
    | Assume (HilExp.AccessExpression access_expr, _, _, _) ->
        assume_pnames_notnull (HilExp.AccessExpression.to_access_path access_expr) astate
    | Assume
        ( ( HilExp.BinaryOperator (Binop.Ne, HilExp.AccessExpression access_expr, exp)
          | HilExp.BinaryOperator (Binop.Ne, exp, HilExp.AccessExpression access_expr) )
        , _
        , _
        , _ )
    | Assume
        ( HilExp.UnaryOperator
            ( Unop.LNot
            , ( HilExp.BinaryOperator (Binop.Eq, HilExp.AccessExpression access_expr, exp)
              | HilExp.BinaryOperator (Binop.Eq, exp, HilExp.AccessExpression access_expr) )
            , _ )
        , _
        , _
        , _ ) ->
        if HilExp.is_null_literal exp then
          assume_pnames_notnull (HilExp.AccessExpression.to_access_path access_expr) astate
        else astate
    | _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "nullability check"
end

module Analyzer = LowerHil.MakeAbstractInterpreter (TransferFunctions (ProcCfg.Exceptional))

let checker {Callbacks.summary; proc_desc; tenv} =
  let initial = (NullableAP.empty, NullCheckedPname.empty) in
  let proc_data = ProcData.make proc_desc tenv summary in
  ignore (Analyzer.compute_post proc_data ~initial) ;
  summary
