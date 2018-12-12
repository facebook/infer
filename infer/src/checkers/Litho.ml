(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module Domain = LithoDomain

module Payload = SummaryPayload.Make (struct
  type t = Domain.t

  let update_payloads astate (payloads : Payloads.t) = {payloads with litho= Some astate}

  let of_payloads (payloads : Payloads.t) = payloads.litho
end)

module LithoFramework = struct
  (** return true if this function is part of the Litho framework code rather than client code *)
  let is_function = function
    | Typ.Procname.Java java_procname -> (
      match Typ.Procname.Java.get_package java_procname with
      | Some "com.facebook.litho" ->
          true
      | _ ->
          false )
    | _ ->
        false


  let is_component_builder procname tenv =
    match procname with
    | Typ.Procname.Java java_procname ->
        PatternMatch.is_subtype_of_str tenv
          (Typ.Procname.Java.get_class_type_name java_procname)
          "com.facebook.litho.Component$Builder"
    | _ ->
        false


  let is_component_build_method procname tenv =
    match Typ.Procname.get_method procname with
    | "build" ->
        is_component_builder procname tenv
    | _ ->
        false


  let is_on_create_layout = function
    | Typ.Procname.Java java_pname -> (
      match Typ.Procname.Java.get_method java_pname with "onCreateLayout" -> true | _ -> false )
    | _ ->
        false
end

module GraphQLGetters = struct
  (* return true if this is a graphql getter *)
  let is_function procname summary =
    Option.is_none summary
    (* we skip analysis of all GraphQL procs *)
    &&
    match procname with
    | Typ.Procname.Java java_procname -> (
        PatternMatch.is_getter java_procname
        &&
        match Typ.Procname.Java.get_package java_procname with
        | Some package ->
            String.is_prefix ~prefix:"com.facebook.graphql.model" package
        | None ->
            false )
    | _ ->
        false


  let should_report proc_desc =
    LithoFramework.is_on_create_layout (Procdesc.get_proc_name proc_desc)


  let report astate summary =
    let report_graphql_getter access_path call_chain =
      let call_strings =
        List.map ~f:(Typ.Procname.to_simplified_string ~withclass:false) call_chain
      in
      let call_string = String.concat ~sep:"." call_strings in
      let message = F.asprintf "%a.%s" AccessPath.pp access_path call_string in
      let loc = Summary.get_loc summary in
      let ltr = [Errlog.make_trace_element 0 loc message []] in
      Reporting.log_error summary ~loc ~ltr IssueType.graphql_field_access message
    in
    Domain.iter_call_chains ~f:report_graphql_getter astate
end

module RequiredProps = struct
  let get_required_props typename tenv =
    let is_required annot_list =
      List.exists
        ~f:(fun ({Annot.class_name; parameters}, _) ->
          String.is_suffix class_name ~suffix:Annotations.prop
          && (* Don't count as required if it's @Prop(optional = true) *)
             not (List.exists ~f:(fun annot_string -> String.equal annot_string "true") parameters)
          )
        annot_list
    in
    match Tenv.lookup tenv typename with
    | Some {fields} ->
        List.filter_map
          ~f:(fun (fieldname, _, annot) ->
            if is_required annot then Some (Typ.Fieldname.Java.get_field fieldname) else None )
          fields
    | None ->
        []


  let report_missing_required_prop summary prop_string parent_typename loc =
    let message =
      F.asprintf "@Prop %s is required for component %s, but is not set before the call to build()"
        prop_string (Typ.Name.name parent_typename)
    in
    let ltr = [Errlog.make_trace_element 0 loc message []] in
    Reporting.log_error summary ~loc ~ltr IssueType.missing_required_prop message


  (* walk backward through [call_chain] and return the first type T <: Component that is not part of
     the Litho framework (i.e., is client code) *)
  let find_client_component_type call_chain =
    List.find_map
      ~f:(fun pname ->
        match pname with
        | Typ.Procname.Java java_pname ->
            Typ.Name.Java.get_outer_class (Typ.Procname.Java.get_class_type_name java_pname)
        | _ ->
            None )
      call_chain


  let should_report proc_desc tenv =
    let pname = Procdesc.get_proc_name proc_desc in
    (not (LithoFramework.is_function pname))
    && (not (LithoFramework.is_component_build_method pname tenv))
    && Procdesc.get_access proc_desc <> PredSymb.Private


  let has_prop prop_set prop =
    String.Set.mem prop_set prop
    (* @Prop(resType = ...) myProp can also be set via myProp(), myPropAttr(), or myPropRes().
       Our annotation parameter parsing is too primitive to identify resType, so just assume
       that all @Prop's can be set any of these 3 ways. *)
    || String.Set.mem prop_set (prop ^ "Attr")
    || String.Set.mem prop_set (prop ^ "Res")


  let report astate tenv summary =
    let check_required_prop_chain _ call_chain =
      let rev_chain = List.rev call_chain in
      match rev_chain with
      | pname :: _ when LithoFramework.is_component_build_method pname tenv -> (
        (* Here, we'll have a type name like MyComponent$Builder in hand. Truncate the $Builder
               part from the typename, then look at the fields of MyComponent to figure out which
               ones are annotated with @Prop *)
        match find_client_component_type call_chain with
        | Some parent_typename ->
            let required_props = get_required_props parent_typename tenv in
            let prop_set = List.map ~f:Typ.Procname.get_method call_chain |> String.Set.of_list in
            List.iter
              ~f:(fun required_prop ->
                if not (has_prop prop_set required_prop) then
                  report_missing_required_prop summary required_prop parent_typename
                    (Summary.get_loc summary) )
              required_props
        | _ ->
            () )
      | _ ->
          ()
    in
    Domain.iter_call_chains ~f:check_required_prop_chain astate
end

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type extras = ProcData.no_extras

  let apply_callee_summary summary_opt caller_pname ret_id_typ actuals astate =
    match summary_opt with
    | Some summary ->
        (* TODO: append paths if the footprint access path is an actual path instead of a var *)
        let f_sub {Domain.LocalAccessPath.access_path= (var, _), _} =
          match Var.get_footprint_index var with
          | Some footprint_index -> (
            match List.nth actuals footprint_index with
            | Some (HilExp.AccessExpression actual_access_expr) ->
                Some
                  (Domain.LocalAccessPath.make
                     (HilExp.AccessExpression.to_access_path actual_access_expr)
                     caller_pname)
            | _ ->
                None )
          | None ->
              if Var.is_return var then
                Some (Domain.LocalAccessPath.make (ret_id_typ, []) caller_pname)
              else None
        in
        Domain.substitute ~f_sub summary |> Domain.join astate
    | None ->
        astate


  let exec_instr astate (proc_data : extras ProcData.t) _ (instr : HilInstr.t) : Domain.t =
    let caller_pname = Procdesc.get_proc_name proc_data.pdesc in
    match instr with
    | Call
        ( return_base
        , Direct (Typ.Procname.Java java_callee_procname as callee_procname)
        , (HilExp.AccessExpression receiver_ae :: _ as actuals)
        , _
        , _ ) ->
        let summary = Payload.read proc_data.pdesc callee_procname in
        let receiver =
          Domain.LocalAccessPath.make
            (HilExp.AccessExpression.to_access_path receiver_ae)
            caller_pname
        in
        if
          ( LithoFramework.is_component_builder callee_procname proc_data.tenv
          (* track Builder's in order to check required prop's *)
          || GraphQLGetters.is_function callee_procname summary
          || (* track GraphQL getters in order to report graphql field accesses *)
             Domain.mem receiver astate
             (* track anything called on a receiver we're already tracking *) )
          && (not (Typ.Procname.Java.is_static java_callee_procname))
          && not
               ( LithoFramework.is_function callee_procname
               && not (LithoFramework.is_function caller_pname) )
          (* don't track Litho client -> Litho framework calls; we want to use the summaries *)
        then
          let return_access_path = Domain.LocalAccessPath.make (return_base, []) caller_pname in
          let return_calls =
            ( try Domain.find return_access_path astate with Caml.Not_found -> Domain.CallSet.empty
            )
            |> Domain.CallSet.add (Domain.MethodCall.make receiver callee_procname)
          in
          Domain.add return_access_path return_calls astate
        else
          (* treat it like a normal call *)
          apply_callee_summary summary caller_pname return_base actuals astate
    | Call (ret_id_typ, Direct callee_procname, actuals, _, _) ->
        let summary = Payload.read proc_data.pdesc callee_procname in
        apply_callee_summary summary caller_pname ret_id_typ actuals astate
    | Assign (lhs_ae, HilExp.AccessExpression rhs_ae, _) -> (
        (* creating an alias for the rhs binding; assume all reads will now occur through the
           alias. this helps us keep track of chains in cases like tmp = getFoo(); x = tmp;
           tmp.getBar() *)
        let lhs_access_path =
          Domain.LocalAccessPath.make (HilExp.AccessExpression.to_access_path lhs_ae) caller_pname
        in
        let rhs_access_path =
          Domain.LocalAccessPath.make (HilExp.AccessExpression.to_access_path rhs_ae) caller_pname
        in
        try
          let call_set = Domain.find rhs_access_path astate in
          Domain.remove rhs_access_path astate |> Domain.add lhs_access_path call_set
        with Caml.Not_found -> astate )
    | _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "litho"
end

module Analyzer = LowerHil.MakeAbstractInterpreter (TransferFunctions (ProcCfg.Exceptional))

let checker {Callbacks.summary; proc_desc; tenv} =
  let proc_data = ProcData.make_default proc_desc tenv in
  match Analyzer.compute_post proc_data ~initial:Domain.empty with
  | Some post ->
      if RequiredProps.should_report proc_desc tenv then RequiredProps.report post tenv summary ;
      if GraphQLGetters.should_report proc_desc then GraphQLGetters.report post summary ;
      let postprocess astate formal_map : Domain.t =
        let f_sub access_path = Domain.LocalAccessPath.to_formal_option access_path formal_map in
        Domain.substitute ~f_sub astate
      in
      let payload = postprocess post (FormalMap.make proc_desc) in
      Payload.update_summary payload summary
  | None ->
      summary
