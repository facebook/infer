(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format
module L = Logging

(** Create a taint analysis from a specification *)
module Make (TaintSpecification : TaintSpec.S) = struct
  module TraceDomain = TaintSpecification.Trace
  module TaintDomain = TaintSpecification.AccessTree

  module Summary = Summary.Make (struct
    type payload = QuandarySummary.t

    let update_payload quandary_payload (summary: Specs.summary) =
      {summary with payload= {summary.payload with quandary= Some quandary_payload}}

    let read_payload (summary: Specs.summary) = summary.payload.quandary
  end)

  module Domain = TaintDomain

  type extras = {formal_map: FormalMap.t; summary: Specs.summary}

  module TransferFunctions (CFG : ProcCfg.S) = struct
    module CFG = CFG
    module Domain = Domain

    type nonrec extras = extras

    (* get the node associated with [access_path] in [access_tree] *)
    let access_path_get_node access_path access_tree (proc_data: extras ProcData.t) =
      match TaintDomain.get_node access_path access_tree with
      | Some _ as node_opt
       -> node_opt
      | None
       -> let make_footprint_trace footprint_ap =
            let trace =
              TraceDomain.of_source
                (TraceDomain.Source.make_footprint footprint_ap proc_data.pdesc)
            in
            Some (TaintDomain.make_normal_leaf trace)
          in
          let root, _ = AccessPath.Abs.extract access_path in
          match FormalMap.get_formal_index root proc_data.extras.formal_map with
          | Some formal_index
           -> make_footprint_trace (AccessPath.Abs.to_footprint formal_index access_path)
          | None
           -> if Var.is_global (fst root) then make_footprint_trace access_path else None

    (* get the trace associated with [access_path] in [access_tree]. *)
    let access_path_get_trace access_path access_tree proc_data =
      match access_path_get_node access_path access_tree proc_data with
      | Some (trace, _)
       -> trace
      | None
       -> TraceDomain.empty

    let exp_get_node_ ~abstracted raw_access_path access_tree proc_data =
      let access_path =
        if abstracted then AccessPath.Abs.Abstracted raw_access_path
        else AccessPath.Abs.Exact raw_access_path
      in
      access_path_get_node access_path access_tree proc_data

    (* get the node associated with [exp] in [access_tree] *)
    let hil_exp_get_node ?(abstracted= false) (exp: HilExp.t) access_tree proc_data =
      match exp with
      | AccessPath access_path
       -> exp_get_node_ ~abstracted access_path access_tree proc_data
      | _
       -> None

    let add_return_source source ret_base access_tree =
      let trace = TraceDomain.of_source source in
      let id_ap = AccessPath.Abs.Exact (ret_base, []) in
      TaintDomain.add_trace id_ap trace access_tree

    let add_actual_source source index actuals access_tree proc_data =
      match List.nth_exn actuals index with
      | HilExp.AccessPath actual_ap_raw
       -> let actual_ap = AccessPath.Abs.Exact actual_ap_raw in
          let trace = access_path_get_trace actual_ap access_tree proc_data in
          TaintDomain.add_trace actual_ap (TraceDomain.add_source source trace) access_tree
      | _
       -> access_tree
      | exception Failure _
       -> failwithf "Bad source specification: index %d out of bounds" index

    let endpoints =
      (lazy (String.Set.of_list (QuandaryConfig.Endpoint.of_json Config.quandary_endpoints)))

    let is_endpoint source =
      match CallSite.pname (TraceDomain.Source.call_site source) with
      | Typ.Procname.Java java_pname
       -> String.Set.mem (Lazy.force endpoints) (Typ.Procname.java_get_class_name java_pname)
      | _
       -> false

    (** log any new reportable source-sink flows in [trace] *)
    let report_trace ?(sink_indexes= IntSet.empty) trace cur_site (proc_data: extras ProcData.t) =
      let get_summary pname =
        if Typ.Procname.equal pname (Procdesc.get_proc_name proc_data.pdesc) then
          (* read_summary will trigger ondemand analysis of the current proc. we don't want that. *)
          TaintDomain.empty
        else
          match Summary.read_summary proc_data.pdesc pname with
          | Some summary
           -> TaintSpecification.of_summary_access_tree summary
          | None
           -> TaintDomain.empty
      in
      let get_short_trace_string original_source final_sink =
        F.asprintf "%a -> %a%s" TraceDomain.Source.pp original_source TraceDomain.Sink.pp
          final_sink
          (if is_endpoint original_source then ". Note: source is an endpoint." else "")
      in
      let report_one (source, sink, _) =
        let open TraceDomain in
        let rec expand_source source0 (report_acc, seen_acc as acc) =
          let kind = Source.kind source0 in
          let call_site = Source.call_site source0 in
          let seen_acc' = CallSite.Set.add call_site seen_acc in
          let is_recursive source = CallSite.Set.mem (Source.call_site source) seen_acc' in
          let matching_sources =
            (* TODO: group by matching call sites, remember all access paths *)
            TaintDomain.trace_fold
              (fun acc access_path trace ->
                match
                  List.find
                    ~f:(fun source ->
                      [%compare.equal : Source.Kind.t] kind (Source.kind source)
                      && not (is_recursive source))
                    (Sources.elements (sources trace))
                with
                | Some matching_source
                 -> (Some access_path, matching_source) :: acc
                | None
                 -> acc)
              (get_summary (CallSite.pname call_site))
              []
          in
          match matching_sources with
          | (_, matching_source as choice) :: _
           -> expand_source matching_source (choice :: report_acc, seen_acc')
          | []
           -> acc
        in
        let rec expand_sink sink0 indexes0 (report_acc, seen_acc as acc) =
          let kind = Sink.kind sink0 in
          let call_site = Sink.call_site sink0 in
          let seen_acc' = CallSite.Set.add call_site seen_acc in
          let is_recursive sink = CallSite.Set.mem (Sink.call_site sink) seen_acc' in
          let matching_sinks =
            TaintDomain.trace_fold
              (fun acc _ trace ->
                match
                  List.find
                    ~f:(fun sink ->
                      [%compare.equal : Sink.Kind.t] kind (Sink.kind sink)
                      && not (is_recursive sink))
                    (Sinks.elements (sinks trace))
                with
                | Some matching_sink
                 -> let indexes_match =
                      not (IntSet.is_empty (IntSet.inter indexes0 (get_footprint_indexes trace)))
                    in
                    (matching_sink, indexes_match) :: acc
                | None
                 -> acc)
              (get_summary (CallSite.pname call_site))
              []
          in
          try
            (* try to find a sink whose indexes match the current sink *)
            let matching_sink, _ = List.find_exn ~f:snd matching_sinks in
            expand_sink matching_sink (Sink.indexes matching_sink)
              (matching_sink :: report_acc, seen_acc')
          with Not_found ->
            (* didn't find a sink whose indexes match; this can happen when taint flows in via a
               global. pick any sink whose kind matches *)
            match matching_sinks with
            | (matching_sink, _) :: _
             -> expand_sink matching_sink (Sink.indexes matching_sink)
                  (matching_sink :: report_acc, seen_acc')
            | []
             -> acc
        in
        let expanded_sources, _ = expand_source source ([(None, source)], CallSite.Set.empty) in
        let expanded_sinks, _ = expand_sink sink sink_indexes ([sink], CallSite.Set.empty) in
        let source_trace =
          let pp_access_path_opt fmt = function
            | None
             -> F.fprintf fmt ""
            | Some access_path
             -> let base, _ = AccessPath.Abs.extract access_path in
                F.fprintf fmt " with tainted data %a" AccessPath.Abs.pp
                  ( if Var.is_footprint (fst base) then
                      (* TODO: resolve footprint identifier to formal name *)
                      access_path
                  else access_path )
          in
          List.map
            ~f:(fun (access_path_opt, source) ->
              let call_site = Source.call_site source in
              let desc =
                Format.asprintf "Return from %a%a" Typ.Procname.pp (CallSite.pname call_site)
                  pp_access_path_opt access_path_opt
              in
              Errlog.make_trace_element 0 (CallSite.loc call_site) desc [])
            expanded_sources
        in
        let sink_trace =
          List.map
            ~f:(fun sink ->
              let call_site = Sink.call_site sink in
              let desc = Format.asprintf "Call to %a" Typ.Procname.pp (CallSite.pname call_site) in
              Errlog.make_trace_element 0 (CallSite.loc call_site) desc [])
            expanded_sinks
        in
        let msg = Localise.to_issue_id Localise.quandary_taint_error in
        let _, original_source = List.hd_exn expanded_sources in
        let final_sink = List.hd_exn expanded_sinks in
        let trace_str = get_short_trace_string original_source final_sink in
        let ltr = source_trace @ List.rev sink_trace in
        let exn = Exceptions.Checkers (msg, Localise.verbatim_desc trace_str) in
        Reporting.log_error proc_data.extras.summary ~loc:(CallSite.loc cur_site) ~ltr exn
      in
      List.iter ~f:report_one (TraceDomain.get_reports ~cur_site trace)

    let add_sink sink actuals access_tree proc_data callee_site =
      (* add [sink] to the trace associated with the [formal_index]th actual *)
      let add_sink_to_actual sink_index access_tree_acc =
        match List.nth actuals sink_index with
        | Some HilExp.AccessPath actual_ap_raw
         -> (
            let actual_ap = AccessPath.Abs.Abstracted actual_ap_raw in
            match access_path_get_node actual_ap access_tree_acc proc_data with
            | Some (actual_trace, _)
             -> let sink' =
                  let indexes = TraceDomain.get_footprint_indexes actual_trace in
                  TraceDomain.Sink.make ~indexes (TraceDomain.Sink.kind sink) callee_site
                in
                let actual_trace' = TraceDomain.add_sink sink' actual_trace in
                report_trace actual_trace' callee_site proc_data ;
                TaintDomain.add_trace actual_ap actual_trace' access_tree_acc
            | None
             -> access_tree_acc )
        | None
         -> failwithf
              "Taint is supposed to flow into sink %a at index %d, but the index is out of bounds@\n"
              CallSite.pp callee_site sink_index
        | _
         -> access_tree_acc
      in
      IntSet.fold add_sink_to_actual (TraceDomain.Sink.indexes sink) access_tree

    let apply_summary ret_opt (actuals: HilExp.t list) summary caller_access_tree
        (proc_data: extras ProcData.t) callee_site =
      let get_caller_ap formal_ap =
        let apply_return ret_ap =
          match ret_opt with
          | Some base_var
           -> Some (AccessPath.Abs.with_base base_var ret_ap)
          | None
           -> Logging.internal_error "Have summary for retval, but no ret id to bind it to: %a@\n"
                AccessPath.Abs.pp ret_ap ;
              None
        in
        let get_actual_ap formal_index =
          Option.value_map
            ~f:(function HilExp.AccessPath access_path -> Some access_path | _ -> None)
            ~default:None (List.nth actuals formal_index)
        in
        let project ~formal_ap ~actual_ap =
          let projected_ap =
            AccessPath.append actual_ap (snd (AccessPath.Abs.extract formal_ap))
          in
          if AccessPath.Abs.is_exact formal_ap then AccessPath.Abs.Exact projected_ap
          else AccessPath.Abs.Abstracted projected_ap
        in
        let base_var, _ = fst (AccessPath.Abs.extract formal_ap) in
        match base_var with
        | Var.ProgramVar pvar
         -> if Pvar.is_return pvar then apply_return formal_ap else Some formal_ap
        | Var.LogicalVar id when Ident.is_footprint id -> (
          match
            (* summaries store the index of the formal parameter in the ident stamp *)
            get_actual_ap (Ident.get_stamp id)
          with
          | Some actual_ap
           -> let projected_ap = project ~formal_ap ~actual_ap in
              Some projected_ap
          | None
           -> None )
        | _
         -> None
      in
      let get_caller_ap_node_opt ap access_tree =
        let get_caller_node caller_ap =
          let caller_node_opt = access_path_get_node caller_ap access_tree proc_data in
          let caller_node = Option.value ~default:TaintDomain.empty_node caller_node_opt in
          (caller_ap, caller_node)
        in
        Option.map (get_caller_ap ap) ~f:get_caller_node
      in
      let replace_footprint_sources callee_trace caller_trace access_tree =
        let replace_footprint_source source acc =
          match TraceDomain.Source.get_footprint_access_path source with
          | Some footprint_access_path -> (
            match get_caller_ap_node_opt footprint_access_path access_tree with
            | Some (_, (caller_ap_trace, _))
             -> TraceDomain.join caller_ap_trace acc
            | None
             -> acc )
          | None
           -> acc
        in
        TraceDomain.Sources.fold replace_footprint_source (TraceDomain.sources callee_trace)
          caller_trace
      in
      let instantiate_and_report callee_trace caller_trace access_tree =
        let caller_trace' = replace_footprint_sources callee_trace caller_trace access_tree in
        let sink_indexes = TraceDomain.get_footprint_indexes callee_trace in
        let appended_trace = TraceDomain.append caller_trace' callee_trace callee_site in
        report_trace appended_trace callee_site ~sink_indexes proc_data ; appended_trace
      in
      let add_to_caller_tree access_tree_acc callee_ap callee_trace =
        match get_caller_ap_node_opt callee_ap access_tree_acc with
        | Some (caller_ap, (caller_trace, caller_tree))
         -> let trace = instantiate_and_report callee_trace caller_trace access_tree_acc in
            TaintDomain.add_node caller_ap (trace, caller_tree) access_tree_acc
        | None
         -> ignore (instantiate_and_report callee_trace TraceDomain.empty access_tree_acc) ;
            access_tree_acc
      in
      TaintDomain.trace_fold add_to_caller_tree summary caller_access_tree

    let exec_instr (astate: Domain.astate) (proc_data: extras ProcData.t) _ (instr: HilInstr.t) =
      (* not all sinks are function calls; we might want to treat an array or field access as a
         sink too. do this by pretending an access is a call to a dummy function and using the
         existing machinery for adding function call sinks *)
      let add_sinks_for_access_path (_, accesses) loc astate =
        let add_sinks_for_access astate_acc = function
          | AccessPath.FieldAccess _ | AccessPath.ArrayAccess (_, [])
           -> astate_acc
          | AccessPath.ArrayAccess (_, indexes)
           -> let dummy_call_site = CallSite.make BuiltinDecl.__array_access loc in
              let dummy_actuals =
                List.map ~f:(fun index_ap -> HilExp.AccessPath index_ap) indexes
              in
              let sinks =
                TraceDomain.Sink.get dummy_call_site dummy_actuals proc_data.ProcData.tenv
              in
              match sinks with
              | None
               -> astate_acc
              | Some sink
               -> add_sink sink dummy_actuals astate proc_data dummy_call_site
        in
        List.fold ~f:add_sinks_for_access ~init:astate accesses
      in
      let add_sinks_for_exp exp loc astate =
        match exp with
        | HilExp.AccessPath access_path
         -> add_sinks_for_access_path access_path loc astate
        | _
         -> astate
      in
      let exec_write lhs_access_path rhs_exp astate =
        let rhs_node =
          Option.value (hil_exp_get_node rhs_exp astate proc_data) ~default:TaintDomain.empty_node
        in
        TaintDomain.add_node (AccessPath.Abs.Exact lhs_access_path) rhs_node astate
      in
      match instr with
      | Assign (((Var.ProgramVar pvar, _), []), HilExp.Exception _, _) when Pvar.is_return pvar
       -> (* the Java frontend translates `throw Exception` as `return Exception`, which is a bit
               wonky. this translation causes problems for us in computing a summary when an
               exception is "returned" from a void function. skip code like this for now, fix via
               t14159157 later *)
          astate
      | Assign (((Var.ProgramVar pvar, _), []), rhs_exp, _)
        when Pvar.is_return pvar && HilExp.is_null_literal rhs_exp
             && Typ.equal_desc Tvoid (Procdesc.get_ret_type proc_data.pdesc).desc
       -> (* similar to the case above; the Java frontend translates "return no exception" as
             `return null` in a void function *)
          astate
      | Assign (lhs_access_path, rhs_exp, loc)
       -> add_sinks_for_exp rhs_exp loc astate |> add_sinks_for_access_path lhs_access_path loc
          |> exec_write lhs_access_path rhs_exp
      | Assume (assume_exp, _, _, loc)
       -> add_sinks_for_exp assume_exp loc astate
      | Call (ret_opt, Direct called_pname, actuals, call_flags, callee_loc)
       -> let astate =
            List.fold ~f:(fun acc exp -> add_sinks_for_exp exp callee_loc acc) actuals ~init:astate
          in
          let handle_model callee_pname access_tree model =
            let is_variadic =
              match callee_pname with
              | Typ.Procname.Java pname -> (
                match List.rev (Typ.Procname.java_get_parameters pname) with
                | (_, "java.lang.Object[]") :: _
                 -> true
                | _
                 -> false )
              | _
               -> false
            in
            let should_taint_typ typ = is_variadic || TaintSpecification.is_taintable_type typ in
            let exp_join_traces trace_acc exp =
              match hil_exp_get_node ~abstracted:true exp access_tree proc_data with
              | Some (trace, _)
               -> TraceDomain.join trace trace_acc
              | None
               -> trace_acc
            in
            let propagate_to_access_path access_path actuals access_tree =
              let initial_trace = access_path_get_trace access_path access_tree proc_data in
              let trace_with_propagation =
                List.fold ~f:exp_join_traces ~init:initial_trace actuals
              in
              let filtered_sources =
                TraceDomain.Sources.filter
                  (fun source ->
                    match TraceDomain.Source.get_footprint_access_path source with
                    | Some access_path
                     -> Option.exists
                          (AccessPath.get_typ (AccessPath.Abs.extract access_path) proc_data.tenv)
                          ~f:should_taint_typ
                    | None
                     -> true)
                  (TraceDomain.sources trace_with_propagation)
              in
              if TraceDomain.Sources.is_empty filtered_sources then access_tree
              else
                let trace' = TraceDomain.update_sources trace_with_propagation filtered_sources in
                TaintDomain.add_trace access_path trace' access_tree
            in
            let handle_model_ astate_acc propagation =
              match (propagation, actuals, ret_opt) with
              | _, [], _
               -> astate_acc
              | TaintSpec.Propagate_to_return, actuals, Some ret_ap
               -> propagate_to_access_path (AccessPath.Abs.Exact (ret_ap, [])) actuals astate_acc
              | ( TaintSpec.Propagate_to_receiver
                , (AccessPath receiver_ap) :: (_ :: _ as other_actuals)
                , _ )
               -> propagate_to_access_path (AccessPath.Abs.Exact receiver_ap) other_actuals
                    astate_acc
              | TaintSpec.Propagate_to_actual actual_index, _, _ -> (
                match List.nth actuals actual_index with
                | Some HilExp.AccessPath actual_ap
                 -> propagate_to_access_path (AccessPath.Abs.Exact actual_ap) actuals astate_acc
                | _
                 -> astate_acc )
              | _
               -> astate_acc
            in
            List.fold ~f:handle_model_ ~init:access_tree model
          in
          let handle_unknown_call callee_pname access_tree =
            match Typ.Procname.get_method callee_pname with
            | "operator=" when not (Typ.Procname.is_java callee_pname) -> (
              match (* treat unknown calls to C++ operator= as assignment *)
                    actuals with
              | [(AccessPath lhs_access_path); rhs_exp]
               -> exec_write lhs_access_path rhs_exp access_tree
              | [ (AccessPath lhs_access_path)
                ; rhs_exp
                ; (HilExp.AccessPath ((Var.ProgramVar pvar, _), [] as dummy_ret_access_path)) ]
                when Pvar.is_frontend_tmp pvar
               -> (* the frontend translates operator=(x, y) as operator=(x, y, dummy_ret) when
                     operator= returns a value type *)
                  exec_write lhs_access_path rhs_exp access_tree
                  |> exec_write dummy_ret_access_path rhs_exp
              | _
               -> failwithf "Unexpected call to operator= %a in %a" HilInstr.pp instr
                    Typ.Procname.pp callee_pname )
            | _
             -> let model =
                  TaintSpecification.handle_unknown_call callee_pname (Option.map ~f:snd ret_opt)
                    actuals proc_data.tenv
                in
                handle_model callee_pname access_tree model
          in
          let dummy_ret_opt =
            match ret_opt with
            | None when not (Typ.Procname.is_java called_pname) -> (
              match
                (* the C++ frontend handles returns of non-pointers by adding a dummy
                   pass-by-reference variable as the last actual, then returning the value by
                   assigning to it. understand this pattern by pretending it's the return value *)
                List.last actuals
              with
              | Some HilExp.AccessPath ((Var.ProgramVar pvar, _ as ret_base), [])
                when Pvar.is_frontend_tmp pvar
               -> Some ret_base
              | _
               -> None )
            | _
             -> ret_opt
          in
          let analyze_call astate_acc callee_pname =
            let call_site = CallSite.make callee_pname callee_loc in
            let sink =
              if List.is_empty actuals then None
              else TraceDomain.Sink.get call_site actuals proc_data.ProcData.tenv
            in
            let astate_with_sink =
              match sink with
              | Some sink
               -> add_sink sink actuals astate proc_data call_site
              | None
               -> astate
            in
            let source = TraceDomain.Source.get call_site actuals proc_data.tenv in
            let astate_with_source =
              match source with
              | Some {TraceDomain.Source.source; index= None}
               -> Option.value_map
                    ~f:(fun ret_base -> add_return_source source ret_base astate_with_sink)
                    ~default:astate_with_sink dummy_ret_opt
              | Some {TraceDomain.Source.source; index= Some index}
               -> add_actual_source source index actuals astate_with_sink proc_data
              | None
               -> astate_with_sink
            in
            let astate_with_summary =
              if Option.is_some source || Option.is_some sink then
                (* don't use a summary for a procedure that is a direct source or sink *)
                astate_with_source
              else
                match Summary.read_summary proc_data.pdesc callee_pname with
                | None
                 -> handle_unknown_call callee_pname astate_with_source
                | Some summary
                 -> let ret_typ_opt = Option.map ~f:snd ret_opt in
                    let access_tree = TaintSpecification.of_summary_access_tree summary in
                    match
                      TaintSpecification.get_model callee_pname ret_typ_opt actuals proc_data.tenv
                        access_tree
                    with
                    | Some model
                     -> handle_model callee_pname astate_with_source model
                    | None
                     -> apply_summary ret_opt actuals access_tree astate_with_source proc_data
                          call_site
            in
            let astate_with_sanitizer =
              match dummy_ret_opt with
              | None
               -> astate_with_summary
              | Some ret_base ->
                match TaintSpecification.get_sanitizer callee_pname with
                | Some Return
                 -> (* clear the trace associated with the return value. ideally, we would
                          associate a kind with the sanitizer and only clear the trace when its
                          kind matches the source. but this gets complicated to do properly with
                          footprint sources, since we don't know their kind. so do the simple
                          thing for now. *)
                    TaintDomain.BaseMap.remove ret_base astate_with_summary
                | None
                 -> astate_with_summary
            in
            Domain.join astate_acc astate_with_sanitizer
          in
          (* highly polymorphic call sites stress reactive mode too much by using too much memory.
             here, we choose an arbitrary call limit that allows us to finish the analysis in
             practice. this is obviously unsound; will try to remove in the future. *)
          let max_calls = 3 in
          let targets =
            if List.length call_flags.cf_targets <= max_calls then called_pname
              :: call_flags.cf_targets
            else (
              L.(debug Analysis Medium)
                "Skipping highly polymorphic call site for %a@." Typ.Procname.pp called_pname ;
              [called_pname] )
          in
          (* for each possible target of the call, apply the summary. join all results together *)
          List.fold ~f:analyze_call ~init:Domain.empty targets
      | _
       -> astate
  end

  module HilConfig : LowerHil.HilConfig = struct
    (* we want this so we can treat array accesses as sinks *)
    let include_array_indexes = true
  end

  module Analyzer =
    AbstractInterpreter.Make (ProcCfg.Exceptional) (LowerHil.Make (TransferFunctions) (HilConfig))

  let make_summary {ProcData.pdesc; extras= {formal_map}} access_tree =
    let is_java = Typ.Procname.is_java (Procdesc.get_proc_name pdesc) in
    (* if a trace has footprint sources, attach them to the appropriate footprint var *)
    let access_tree' =
      TaintDomain.fold
        (fun access_tree_acc _ (trace, _ as node) ->
          if TraceDomain.Sinks.is_empty (TraceDomain.sinks trace) then
            (* if this trace has no sinks, we don't need to attach it to anything *)
            access_tree_acc
          else
            TraceDomain.Sources.fold
              (fun source acc ->
                match TraceDomain.Source.get_footprint_access_path source with
                | Some footprint_access_path
                 -> let node' =
                      match TaintDomain.get_node footprint_access_path acc with
                      | Some n
                       -> TaintDomain.node_join node n
                      | None
                       -> node
                    in
                    TaintDomain.add_node footprint_access_path node' acc
                | None
                 -> acc)
              (TraceDomain.sources trace) access_tree_acc)
        access_tree access_tree
    in
    (* should only be used on nodes associated with a footprint base *)
    let is_empty_node (trace, tree) =
      (* In C++, we can reassign the value pointed to by a pointer type formal, and we can assign
         to a value type passed by reference. these mechanisms can be used to associate a source
         directly with a formal. In Java this can't happen, so we only care if the formal flows to
         a sink *)
      ( if is_java then TraceDomain.Sinks.is_empty (TraceDomain.sinks trace)
      else TraceDomain.is_empty trace )
      &&
      match tree with
      | TaintDomain.Subtree subtree
       -> TaintDomain.AccessMap.is_empty subtree
      | TaintDomain.Star
       -> true
    in
    (* replace formal names with footprint vars for their indices. For example, for `foo(o)`, we'll
       replace `o` with FP(1) *)
    let with_footprint_vars =
      AccessPath.BaseMap.fold
        (fun base (trace, subtree as node) acc ->
          if Var.is_global (fst base) || Var.is_return (fst base) then
            AccessPath.BaseMap.add base node acc
          else if Var.is_footprint (fst base) then
            if is_empty_node node then acc
            else
              let node' =
                if TraceDomain.Sinks.is_empty (TraceDomain.sinks trace) then
                  (TraceDomain.empty, subtree)
                else node
              in
              AccessPath.BaseMap.add base node' acc
          else
            match FormalMap.get_formal_index base formal_map with
            | Some formal_index
             -> let base' = (Var.of_formal_index formal_index, snd base) in
                let joined_node =
                  try TaintDomain.node_join (AccessPath.BaseMap.find base' acc) node
                  with Not_found -> node
                in
                if is_empty_node joined_node then acc
                else AccessPath.BaseMap.add base' joined_node acc
            | None
             -> (* base is a local var *)
                acc)
        access_tree' TaintDomain.empty
    in
    TaintSpecification.to_summary_access_tree with_footprint_vars

  let checker {Callbacks.tenv; summary; proc_desc} : Specs.summary =
    (* bind parameters to a trace with a tainted source (if applicable) *)
    let make_initial pdesc =
      let pname = Procdesc.get_proc_name pdesc in
      let access_tree =
        List.fold
          ~f:(fun acc (name, typ, taint_opt) ->
            match taint_opt with
            | Some source
             -> let base_ap = AccessPath.Abs.Exact (AccessPath.of_pvar (Pvar.mk name pname) typ) in
                TaintDomain.add_trace base_ap (TraceDomain.of_source source) acc
            | None
             -> acc)
          ~init:TaintDomain.empty (TraceDomain.Source.get_tainted_formals pdesc tenv)
      in
      (access_tree, IdAccessPathMapDomain.empty)
    in
    if not (Procdesc.did_preanalysis proc_desc) then (
      Preanal.do_liveness proc_desc tenv ;
      Preanal.do_dynamic_dispatch proc_desc (Cg.create (SourceFile.invalid __FILE__)) tenv ) ;
    let initial = make_initial proc_desc in
    let extras =
      let formal_map = FormalMap.make proc_desc in
      {formal_map; summary}
    in
    let proc_data = ProcData.make proc_desc tenv extras in
    match Analyzer.compute_post proc_data ~initial ~debug:false with
    | Some (access_tree, _)
     -> Summary.update_summary (make_summary proc_data access_tree) summary
    | None
     -> if Procdesc.Node.get_succs (Procdesc.get_start_node proc_desc) <> [] then
          failwith "Couldn't compute post"
        else summary
end
