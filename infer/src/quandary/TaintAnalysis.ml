(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

(** Create a taint analysis from a specification *)
module Make (TaintSpecification : TaintSpec.S) = struct
  module TraceDomain = TaintSpecification.Trace
  module TaintDomain = TaintSpecification.AccessTree

  module Payload = SummaryPayload.Make (struct
    type t = QuandarySummary.t

    let update_payloads quandary_payload (payloads : Payloads.t) =
      {payloads with quandary= Some quandary_payload}


    let of_payloads (payloads : Payloads.t) = payloads.quandary
  end)

  module Domain = TaintDomain

  type extras = {formal_map: FormalMap.t; summary: Summary.t}

  module TransferFunctions (CFG : ProcCfg.S) = struct
    module CFG = CFG
    module Domain = Domain

    type nonrec extras = extras

    (* get the node associated with [access_path] in [access_tree] *)
    let access_path_get_node access_path access_tree (proc_data : extras ProcData.t) =
      match TaintDomain.get_node access_path access_tree with
      | Some _ as node_opt ->
          node_opt
      | None -> (
          let make_footprint_trace footprint_ap =
            let trace = TraceDomain.of_footprint footprint_ap in
            Some (TaintDomain.make_normal_leaf trace)
          in
          let root, _ = AccessPath.Abs.extract access_path in
          match FormalMap.get_formal_index root proc_data.extras.formal_map with
          | Some formal_index ->
              make_footprint_trace (AccessPath.Abs.to_footprint formal_index access_path)
          | None ->
              if Var.is_global (fst root) then make_footprint_trace access_path else None )


    (* get the trace associated with [access_path] in [access_tree]. *)
    let access_path_get_trace access_path access_tree proc_data =
      match access_path_get_node access_path access_tree proc_data with
      | Some (trace, _) ->
          trace
      | None ->
          TraceDomain.empty


    let exp_get_node_ ~abstracted raw_access_path access_tree proc_data =
      let access_path =
        if abstracted then AccessPath.Abs.Abstracted raw_access_path
        else AccessPath.Abs.Exact raw_access_path
      in
      access_path_get_node access_path access_tree proc_data


    (* get the node associated with [exp] in [access_tree] *)
    let rec hil_exp_get_node ?(abstracted = false) (exp : HilExp.t) access_tree proc_data =
      match exp with
      | AccessExpression access_expr ->
          exp_get_node_ ~abstracted
            (HilExp.AccessExpression.to_access_path access_expr)
            access_tree proc_data
      | Cast (_, e) | Exception e | UnaryOperator (_, e, _) ->
          hil_exp_get_node ~abstracted e access_tree proc_data
      | BinaryOperator (_, e1, e2) -> (
        match
          ( hil_exp_get_node ~abstracted e1 access_tree proc_data
          , hil_exp_get_node ~abstracted e2 access_tree proc_data )
        with
        | Some node1, Some node2 ->
            Some (TaintDomain.node_join node1 node2)
        | node_opt, None | None, node_opt ->
            node_opt )
      | _ ->
          None


    let add_return_source source ret_base access_tree =
      let trace = TraceDomain.of_source source in
      let id_ap = AccessPath.Abs.Abstracted (ret_base, []) in
      TaintDomain.add_trace id_ap trace access_tree


    let add_actual_source source index actuals access_tree proc_data =
      match HilExp.ignore_cast (List.nth_exn actuals index) with
      | HilExp.AccessExpression actual_ae_raw ->
          let actual_ap =
            AccessPath.Abs.Abstracted (HilExp.AccessExpression.to_access_path actual_ae_raw)
          in
          let trace = access_path_get_trace actual_ap access_tree proc_data in
          TaintDomain.add_trace actual_ap (TraceDomain.add_source source trace) access_tree
      | _ ->
          access_tree
      | exception Failure s ->
          L.internal_error
            "Bad source specification: index %d out of bounds (%s) for source %a, actuals %a" index
            s TraceDomain.Source.pp source
            (PrettyPrintable.pp_collection ~pp_item:HilExp.pp)
            actuals ;
          access_tree


    let is_endpoint source =
      match CallSite.pname (TraceDomain.Source.call_site source) with
      | Typ.Procname.Java java_pname ->
          QuandaryConfig.is_endpoint (Typ.Procname.Java.get_class_name java_pname)
      | _ ->
          false


    (** log any new reportable source-sink flows in [trace] *)
    let report_trace ?(sink_indexes = IntSet.empty) trace cur_site (proc_data : extras ProcData.t)
        =
      let get_summary pname =
        if Typ.Procname.equal pname (Procdesc.get_proc_name proc_data.pdesc) then
          (* read_summary will trigger ondemand analysis of the current proc. we don't want that. *)
          TaintDomain.empty
        else
          match Payload.read proc_data.pdesc pname with
          | Some summary ->
              TaintSpecification.of_summary_access_tree summary
          | None ->
              TaintDomain.empty
      in
      let get_caller_string caller_site =
        let caller_pname = CallSite.pname caller_site in
        F.sprintf " in procedure %s"
          (Typ.Procname.to_simplified_string ~withclass:true caller_pname)
      in
      let pp_trace_elem site fmt caller_string =
        F.fprintf fmt "(%s)%s at %a"
          (Typ.Procname.to_simplified_string ~withclass:true (CallSite.pname site))
          caller_string Location.pp (CallSite.loc site)
      in
      let pp_source source_caller_opt fmt initial_source =
        let source_caller_string =
          match source_caller_opt with
          | Some source_caller ->
              get_caller_string (TraceDomain.Source.call_site source_caller)
          | None ->
              ""
        in
        F.fprintf fmt "%a%a" TraceDomain.Source.Kind.pp
          (TraceDomain.Source.kind initial_source)
          (pp_trace_elem (TraceDomain.Source.call_site initial_source))
          source_caller_string
      in
      let pp_sink sink_caller_opt fmt final_sink =
        let sink_caller_string =
          match sink_caller_opt with
          | Some sink_caller ->
              get_caller_string (TraceDomain.Sink.call_site sink_caller)
          | None ->
              ""
        in
        F.fprintf fmt "%a%a" TraceDomain.Sink.Kind.pp
          (TraceDomain.Sink.kind final_sink)
          (pp_trace_elem (TraceDomain.Sink.call_site final_sink))
          sink_caller_string
      in
      let get_short_trace_string initial_source source_caller_opt final_sink sink_caller_opt =
        F.asprintf "%a ~> %a%s" (pp_source source_caller_opt) initial_source
          (pp_sink sink_caller_opt) final_sink
          (if is_endpoint initial_source then ". Note: source is an endpoint." else "")
      in
      let report_one {TraceDomain.issue; path_source; path_sink} =
        let open TraceDomain in
        let rec expand_source source0 ((report_acc, seen_acc) as acc) =
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
                      [%compare.equal: Source.Kind.t] kind (Source.kind source)
                      && not (is_recursive source) )
                    (Sources.Known.elements (sources trace).Sources.known)
                with
                | Some matching_source ->
                    (Some access_path, matching_source) :: acc
                | None ->
                    acc )
              (get_summary (CallSite.pname call_site))
              []
          in
          match matching_sources with
          | ((_, matching_source) as choice) :: _ ->
              expand_source matching_source (choice :: report_acc, seen_acc')
          | [] ->
              acc
        in
        let rec expand_sink sink0 indexes0 ((report_acc, seen_acc) as acc) =
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
                      [%compare.equal: Sink.Kind.t] kind (Sink.kind sink)
                      && not (is_recursive sink) )
                    (Sinks.elements (sinks trace))
                with
                | Some matching_sink ->
                    let indexes_match =
                      not (IntSet.is_empty (IntSet.inter indexes0 (get_footprint_indexes trace)))
                    in
                    (matching_sink, indexes_match) :: acc
                | None ->
                    acc )
              (get_summary (CallSite.pname call_site))
              []
          in
          (* try to find a sink whose indexes match the current sink *)
          try
            let matching_sink, _ = List.find_exn ~f:snd matching_sinks in
            expand_sink matching_sink (Sink.indexes matching_sink)
              (matching_sink :: report_acc, seen_acc')
          with
          | Not_found_s _ | Caml.Not_found -> (
            (* didn't find a sink whose indexes match; this can happen when taint flows in via a
               global. pick any sink whose kind matches *)
            match matching_sinks with
            | (matching_sink, _) :: _ ->
                expand_sink matching_sink (Sink.indexes matching_sink)
                  (matching_sink :: report_acc, seen_acc')
            | [] ->
                acc )
        in
        let expanded_sources, _ =
          expand_source path_source ([(None, path_source)], CallSite.Set.empty)
        in
        let expanded_sinks, _ =
          expand_sink path_sink sink_indexes ([path_sink], CallSite.Set.empty)
        in
        let source_trace =
          let pp_access_path_opt fmt = function
            | None ->
                ()
            | Some access_path ->
                let base, _ = AccessPath.Abs.extract access_path in
                F.fprintf fmt " with tainted data %a" AccessPath.Abs.pp
                  ( if Var.is_footprint (fst base) then
                    (* TODO: resolve footprint identifier to formal name *)
                    access_path
                  else access_path )
          in
          List.map
            ~f:(fun (access_path_opt, path_source) ->
              let desc, loc =
                let call_site = Source.call_site path_source in
                ( Format.asprintf "Return from %a%a" Typ.Procname.pp (CallSite.pname call_site)
                    pp_access_path_opt access_path_opt
                , CallSite.loc call_site )
              in
              Errlog.make_trace_element 0 loc desc [] )
            expanded_sources
        in
        let sink_trace =
          List.map
            ~f:(fun sink ->
              let call_site = Sink.call_site sink in
              let indexes = Sink.indexes sink in
              let indexes_str =
                match IntSet.cardinal indexes with
                | 0 ->
                    ""
                | 1 ->
                    " with tainted index " ^ string_of_int (IntSet.choose indexes)
                | _ ->
                    Format.asprintf " with tainted indexes %a"
                      (PrettyPrintable.pp_collection ~pp_item:Int.pp)
                      (IntSet.elements indexes)
              in
              let desc =
                Format.asprintf "Call to %a%s" Typ.Procname.pp (CallSite.pname call_site)
                  indexes_str
              in
              Errlog.make_trace_element 0 (CallSite.loc call_site) desc [] )
            expanded_sinks
        in
        let _, initial_source = List.hd_exn expanded_sources in
        let initial_source_caller = Option.map ~f:snd (List.nth expanded_sources 1) in
        let final_sink = List.hd_exn expanded_sinks in
        let final_sink_caller = List.nth expanded_sinks 1 in
        let trace_str =
          get_short_trace_string initial_source initial_source_caller final_sink final_sink_caller
        in
        let ltr = source_trace @ List.rev sink_trace in
        Reporting.log_error proc_data.extras.summary ~loc:(CallSite.loc cur_site) ~ltr issue
          trace_str
      in
      List.iter ~f:report_one (TraceDomain.get_reports ~cur_site trace)


    let add_sink sink actuals access_tree proc_data callee_site =
      (* add [sink] to the trace associated with the [formal_index]th actual *)
      let add_sink_to_actual sink_index access_tree_acc =
        match List.nth actuals sink_index with
        | Some exp -> (
          match hil_exp_get_node ~abstracted:true exp access_tree_acc proc_data with
          | Some (actual_trace, _) -> (
              let sink' =
                let indexes = IntSet.singleton sink_index in
                TraceDomain.Sink.make ~indexes (TraceDomain.Sink.kind sink) callee_site
              in
              let actual_trace' = TraceDomain.add_sink sink' actual_trace in
              report_trace actual_trace' callee_site proc_data ;
              match HilExp.ignore_cast exp with
              | HilExp.AccessExpression actual_ae_raw
                when not
                       (TraceDomain.Sources.Footprint.is_empty
                          (TraceDomain.sources actual_trace').footprint) ->
                  let actual_ap =
                    AccessPath.Abs.Abstracted
                      (HilExp.AccessExpression.to_access_path actual_ae_raw)
                  in
                  TaintDomain.add_trace actual_ap actual_trace' access_tree_acc
              | _ ->
                  (* no more sources can flow into this sink; no sense in keeping track of it *)
                  access_tree_acc )
          | _ ->
              access_tree_acc )
        | None ->
            L.internal_error
              "Taint is supposed to flow into sink %a at index %d, but the index is out of bounds"
              CallSite.pp callee_site sink_index ;
            access_tree_acc
      in
      IntSet.fold add_sink_to_actual (TraceDomain.Sink.indexes sink) access_tree


    let apply_summary ret_opt (actuals : HilExp.t list) summary caller_access_tree
        (proc_data : extras ProcData.t) callee_site =
      let get_caller_ap_node_opt formal_ap access_tree =
        let apply_return ret_ap =
          match ret_opt with
          | Some base_var ->
              Some (AccessPath.Abs.with_base base_var ret_ap)
          | None ->
              (* TODO (T23832636): fail hard here *)
              None
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
        | Var.ProgramVar pvar ->
            let projected_ap_opt =
              if Pvar.is_return pvar then apply_return formal_ap else Some formal_ap
            in
            let caller_node_opt =
              match projected_ap_opt with
              | Some projected_ap ->
                  access_path_get_node projected_ap access_tree proc_data
              | None ->
                  None
            in
            (projected_ap_opt, Option.value ~default:TaintDomain.empty_node caller_node_opt)
        | Var.LogicalVar id when Ident.is_footprint id -> (
          match Option.map (List.nth actuals (Ident.get_stamp id)) ~f:HilExp.ignore_cast with
          | Some (HilExp.AccessExpression actual_ae) ->
              let projected_ap =
                project ~formal_ap ~actual_ap:(HilExp.AccessExpression.to_access_path actual_ae)
              in
              let caller_node_opt = access_path_get_node projected_ap access_tree proc_data in
              (Some projected_ap, Option.value ~default:TaintDomain.empty_node caller_node_opt)
          | Some exp ->
              let caller_node_opt = hil_exp_get_node exp access_tree proc_data in
              (None, Option.value ~default:TaintDomain.empty_node caller_node_opt)
          | _ ->
              (None, TaintDomain.empty_node) )
        | _ ->
            (None, TaintDomain.empty_node)
      in
      let replace_footprint_sources callee_trace caller_trace access_tree =
        let replace_footprint_source acc footprint_access_path (is_mem, _) =
          if is_mem then
            let _, (caller_ap_trace, _) =
              get_caller_ap_node_opt footprint_access_path access_tree
            in
            TraceDomain.join caller_ap_trace acc
          else acc
        in
        let {TraceDomain.Sources.footprint} = TraceDomain.sources callee_trace in
        TraceDomain.Sources.Footprint.fold replace_footprint_source footprint caller_trace
      in
      let instantiate_and_report callee_trace caller_trace access_tree =
        let caller_trace' = replace_footprint_sources callee_trace caller_trace access_tree in
        let sink_indexes = TraceDomain.get_footprint_indexes callee_trace in
        let appended_trace = TraceDomain.append caller_trace' callee_trace callee_site in
        report_trace appended_trace callee_site ~sink_indexes proc_data ;
        appended_trace
      in
      let add_to_caller_tree access_tree_acc callee_ap callee_trace =
        let caller_ap_opt, (caller_trace, caller_tree) =
          get_caller_ap_node_opt callee_ap access_tree_acc
        in
        let trace = instantiate_and_report callee_trace caller_trace access_tree_acc in
        let pruned_trace =
          if TraceDomain.Sources.Footprint.is_empty (TraceDomain.sources trace).footprint then
            (* empty footprint; nothing else can flow into these sinks. so don't track them *)
            TraceDomain.update_sinks trace TraceDomain.Sinks.empty
          else trace
        in
        match caller_ap_opt with
        | Some caller_ap ->
            TaintDomain.add_node caller_ap (pruned_trace, caller_tree) access_tree_acc
        | None ->
            access_tree_acc
      in
      TaintDomain.trace_fold add_to_caller_tree summary caller_access_tree


    (* not all sinks are function calls; we might want to treat an array or field access as a
           sink too. do this by pretending an access is a call to a dummy function and using the
           existing machinery for adding function call sinks *)
    let add_sinks_for_access_path (proc_data : extras ProcData.t) access_expr loc astate =
      let rec add_sinks_for_access astate_acc = function
        | HilExp.AccessExpression.Base _ ->
            astate_acc
        | HilExp.AccessExpression.FieldOffset (ae, _)
        | ArrayOffset (ae, _, None)
        | AddressOf ae
        | Dereference ae ->
            add_sinks_for_access astate_acc ae
        | HilExp.AccessExpression.ArrayOffset (ae, _, Some index) ->
            let dummy_call_site = CallSite.make BuiltinDecl.__array_access loc in
            let dummy_actuals =
              List.map
                ~f:(fun index_ae -> HilExp.AccessExpression index_ae)
                (HilExp.get_access_exprs index)
            in
            let sinks =
              TraceDomain.Sink.get dummy_call_site dummy_actuals CallFlags.default
                proc_data.ProcData.tenv
            in
            let astate_acc_result =
              List.fold sinks ~init:astate_acc ~f:(fun astate sink ->
                  add_sink sink dummy_actuals astate proc_data dummy_call_site )
            in
            add_sinks_for_access astate_acc_result ae
      in
      add_sinks_for_access astate access_expr


    let add_sources_for_access_path (proc_data : extras ProcData.t) access_expr loc astate =
      let var, _ = HilExp.AccessExpression.get_base access_expr in
      if Var.is_global var then
        let dummy_call_site = CallSite.make BuiltinDecl.__global_access loc in
        let sources =
          let caller_pname = Procdesc.get_proc_name proc_data.ProcData.pdesc in
          TraceDomain.Source.get ~caller_pname dummy_call_site
            [HilExp.AccessExpression access_expr]
            proc_data.tenv
        in
        List.fold sources ~init:astate ~f:(fun astate {TraceDomain.Source.source} ->
            let access_path =
              AccessPath.Abs.Exact (HilExp.AccessExpression.to_access_path access_expr)
            in
            let trace, subtree =
              Option.value ~default:TaintDomain.empty_node
                (TaintDomain.get_node access_path astate)
            in
            TaintDomain.add_node access_path (TraceDomain.add_source source trace, subtree) astate
        )
      else astate


    let rec add_sources_sinks_for_exp (proc_data : extras ProcData.t) exp loc astate =
      match exp with
      | HilExp.Cast (_, e) ->
          add_sources_sinks_for_exp proc_data e loc astate
      | HilExp.AccessExpression access_expr ->
          add_sinks_for_access_path proc_data access_expr loc astate
          |> add_sources_for_access_path proc_data access_expr loc
      | _ ->
          astate


    let exec_write (proc_data : extras ProcData.t) lhs_access_expr rhs_exp astate =
      let rhs_node =
        Option.value (hil_exp_get_node rhs_exp astate proc_data) ~default:TaintDomain.empty_node
      in
      let lhs_access_path = HilExp.AccessExpression.to_access_path lhs_access_expr in
      TaintDomain.add_node (AccessPath.Abs.Exact lhs_access_path) rhs_node astate


    let analyze_call (proc_data : extras ProcData.t) ~ret_ap ~callee_pname ~actuals ~call_flags
        ~callee_loc astate =
      let astate =
        List.fold
          ~f:(fun acc exp -> add_sources_sinks_for_exp proc_data exp callee_loc acc)
          actuals ~init:astate
      in
      let handle_model callee_pname access_tree model =
        let is_variadic =
          match callee_pname with
          | Typ.Procname.Java pname ->
              Typ.Procname.Java.is_vararg pname
          | _ ->
              false
        in
        let should_taint_typ typ = is_variadic || TaintSpecification.is_taintable_type typ in
        let exp_join_traces trace_acc exp =
          match hil_exp_get_node ~abstracted:true exp access_tree proc_data with
          | Some (trace, _) ->
              TraceDomain.join trace trace_acc
          | None ->
              trace_acc
        in
        let propagate_to_access_path access_path actuals access_tree =
          let initial_trace = access_path_get_trace access_path access_tree proc_data in
          let trace_with_propagation = List.fold ~f:exp_join_traces ~init:initial_trace actuals in
          let sources = TraceDomain.sources trace_with_propagation in
          let filtered_footprint =
            TraceDomain.Sources.Footprint.fold
              (fun acc access_path (is_mem, _) ->
                if
                  is_mem
                  && Option.exists
                       (AccessPath.get_typ (AccessPath.Abs.extract access_path) proc_data.tenv)
                       ~f:should_taint_typ
                then TraceDomain.Sources.Footprint.add_trace access_path true acc
                else acc )
              sources.footprint TraceDomain.Sources.Footprint.empty
          in
          let filtered_sources = {sources with footprint= filtered_footprint} in
          if TraceDomain.Sources.is_empty filtered_sources then access_tree
          else
            let trace' = TraceDomain.update_sources trace_with_propagation filtered_sources in
            let pruned_trace =
              if TraceDomain.Sources.Footprint.is_empty filtered_footprint then
                (* empty footprint; nothing else can flow into these sinks. so don't track them *)
                TraceDomain.update_sinks trace' TraceDomain.Sinks.empty
              else trace'
            in
            TaintDomain.add_trace access_path pruned_trace access_tree
        in
        let handle_model_ astate_acc propagation =
          match (propagation, actuals) with
          | _, [] ->
              astate_acc
          | TaintSpec.Propagate_to_return, actuals ->
              propagate_to_access_path (AccessPath.Abs.Abstracted (ret_ap, [])) actuals astate_acc
          | ( TaintSpec.Propagate_to_receiver
            , HilExp.AccessExpression receiver_ae :: (_ :: _ as other_actuals) ) ->
              propagate_to_access_path
                (AccessPath.Abs.Abstracted (HilExp.AccessExpression.to_access_path receiver_ae))
                other_actuals astate_acc
          | TaintSpec.Propagate_to_actual actual_index, _ -> (
            match Option.map (List.nth actuals actual_index) ~f:HilExp.ignore_cast with
            | Some (HilExp.AccessExpression actual_ae) ->
                propagate_to_access_path
                  (AccessPath.Abs.Abstracted (HilExp.AccessExpression.to_access_path actual_ae))
                  actuals astate_acc
            | _ ->
                astate_acc )
          | _ ->
              astate_acc
        in
        List.fold ~f:handle_model_ ~init:access_tree model
      in
      let handle_unknown_call callee_pname access_tree =
        match Typ.Procname.get_method callee_pname with
        | "operator=" when not (Typ.Procname.is_java callee_pname) -> (
          (* treat unknown calls to C++ operator= as assignment *)
          match List.map actuals ~f:HilExp.ignore_cast with
          | [AccessExpression lhs_access_expr; rhs_exp] ->
              exec_write proc_data lhs_access_expr rhs_exp access_tree
          | [AccessExpression lhs_access_expr; rhs_exp; HilExp.AccessExpression access_expr] -> (
              let dummy_ret_access_expr = access_expr in
              match dummy_ret_access_expr with
              | HilExp.AccessExpression.Base (Var.ProgramVar pvar, _)
                when Pvar.is_frontend_tmp pvar ->
                  (* the frontend translates operator=(x, y) as operator=(x, y, dummy_ret) when
                     operator= returns a value type *)
                  exec_write proc_data lhs_access_expr rhs_exp access_tree
                  |> exec_write proc_data dummy_ret_access_expr rhs_exp
              | _ ->
                  L.internal_error "Unexpected call to operator= at %a" Location.pp callee_loc ;
                  access_tree )
          | _ ->
              L.internal_error "Unexpected call to operator= at %a" Location.pp callee_loc ;
              access_tree )
        | _ ->
            let model =
              TaintSpecification.handle_unknown_call callee_pname (snd ret_ap) actuals
                proc_data.tenv
            in
            handle_model callee_pname access_tree model
      in
      let dummy_ret_opt =
        match ret_ap with
        | _, {Typ.desc= Tvoid} when not (Typ.Procname.is_java callee_pname) -> (
          (* the C++ frontend handles returns of non-pointers by adding a dummy
                   pass-by-reference variable as the last actual, then returning the value by
                   assigning to it. understand this pattern by pretending it's the return value *)
          match List.last actuals with
          | Some (HilExp.AccessExpression access_expr) -> (
            match HilExp.AccessExpression.to_access_path access_expr with
            | ((Var.ProgramVar pvar, _) as ret_base), [] when Pvar.is_frontend_tmp pvar ->
                Some ret_base
            | _ ->
                None )
          | _ ->
              None )
        | _ ->
            Some ret_ap
      in
      let call_site = CallSite.make callee_pname callee_loc in
      let astate_with_sink =
        if List.is_empty actuals then astate
        else
          let sinks = TraceDomain.Sink.get call_site actuals call_flags proc_data.ProcData.tenv in
          List.fold sinks ~init:astate ~f:(fun astate sink ->
              add_sink sink actuals astate proc_data call_site )
      in
      let astate_with_direct_sources =
        let sources =
          let caller_pname = Procdesc.get_proc_name proc_data.ProcData.pdesc in
          TraceDomain.Source.get ~caller_pname call_site actuals proc_data.tenv
        in
        List.fold sources ~init:astate_with_sink
          ~f:(fun astate {TraceDomain.Source.source; index} ->
            match index with
            | None ->
                Option.value_map dummy_ret_opt ~default:astate ~f:(fun ret_base ->
                    add_return_source source ret_base astate )
            | Some index ->
                add_actual_source source index actuals astate_with_sink proc_data )
      in
      let astate_with_summary =
        match Payload.read proc_data.pdesc callee_pname with
        | None ->
            handle_unknown_call callee_pname astate_with_direct_sources
        | Some summary -> (
            let ret_typ = snd ret_ap in
            let access_tree = TaintSpecification.of_summary_access_tree summary in
            match
              TaintSpecification.get_model callee_pname ret_typ actuals proc_data.tenv access_tree
            with
            | Some model ->
                handle_model callee_pname astate_with_direct_sources model
            | None ->
                apply_summary dummy_ret_opt actuals access_tree astate_with_direct_sources
                  proc_data call_site )
      in
      let astate_with_sanitizer =
        match dummy_ret_opt with
        | None ->
            astate_with_summary
        | Some ret_base -> (
          match TraceDomain.Sanitizer.get callee_pname proc_data.tenv with
          | Some sanitizer ->
              let ret_ap = AccessPath.Abs.Exact (ret_base, []) in
              let ret_trace = access_path_get_trace ret_ap astate_with_summary proc_data in
              let ret_trace' = TraceDomain.add_sanitizer sanitizer ret_trace in
              TaintDomain.add_trace ret_ap ret_trace' astate_with_summary
          | None ->
              astate_with_summary )
      in
      astate_with_sanitizer


    let exec_instr (astate : Domain.t) (proc_data : extras ProcData.t) _ (instr : HilInstr.t) =
      match instr with
      | Assign (Base (Var.ProgramVar pvar, _), HilExp.Exception _, _) when Pvar.is_return pvar ->
          (* the Java frontend translates `throw Exception` as `return Exception`, which is a bit
               wonky. this translation causes problems for us in computing a summary when an
               exception is "returned" from a void function. skip code like this for now, fix via
               t14159157 later *)
          astate
      | Assign (Base (Var.ProgramVar pvar, _), rhs_exp, _)
        when Pvar.is_return pvar && HilExp.is_null_literal rhs_exp
             && Typ.equal_desc Tvoid (Procdesc.get_ret_type proc_data.pdesc).desc ->
          (* similar to the case above; the Java frontend translates "return no exception" as
             `return null` in a void function *)
          astate
      | Assign (lhs_access_expr, rhs_exp, loc) ->
          add_sources_sinks_for_exp proc_data rhs_exp loc astate
          |> add_sinks_for_access_path proc_data lhs_access_expr loc
          |> exec_write proc_data lhs_access_expr rhs_exp
      | Assume (assume_exp, _, _, loc) ->
          add_sources_sinks_for_exp proc_data assume_exp loc astate
      | Call (ret_ap, Direct callee_pname, actuals, call_flags, callee_loc) ->
          analyze_call proc_data ~ret_ap ~callee_pname ~actuals ~call_flags ~callee_loc astate
      | _ ->
          astate


    let pp_session_name =
      let name = F.sprintf "quandary(%s)" TaintSpecification.name in
      fun (_node : CFG.Node.t) fmt -> F.pp_print_string fmt name
  end

  module HilConfig : LowerHil.HilConfig = struct
    (* we want this so we can treat array accesses as sinks *)
    let include_array_indexes = true
  end

  module Analyzer =
    LowerHil.MakeAbstractInterpreterWithConfig (AbstractInterpreter.MakeRPO) (HilConfig)
      (TransferFunctions (ProcCfg.Exceptional))

  (* sanity checks for summaries. should only be used in developer mode *)
  let check_invariants access_tree =
    let open TraceDomain in
    TaintDomain.iter
      (fun access_path (trace, _) ->
        let sources = sources trace in
        let footprint_sources = sources.footprint in
        let passthroughs = passthroughs trace in
        let sinks = sinks trace in
        (* invariant 1: sinks with no footprint sources are dead and should be forgotten *)
        if Sources.Footprint.is_empty footprint_sources && not (Sinks.is_empty sinks) then
          Logging.die InternalError
            "Trace %a associated with %a tracks sinks even though no more sources can flow into \
             them"
            Sinks.pp sinks AccessPath.Abs.pp access_path ;
        (* invariant 2: we should never have sinks without sources *)
        if Sources.is_empty sources && not (Sinks.is_empty sinks) then
          Logging.die InternalError "We have sinks %a associated with %a, but no sources" Sinks.pp
            sinks AccessPath.Abs.pp access_path ;
        (* invariant 3: we should never have passthroughs without sources *)
        if Sources.is_empty sources && not (Passthroughs.is_empty passthroughs) then
          Logging.die InternalError "We have passthroughs %a associated with %a, but no sources"
            Passthroughs.pp passthroughs AccessPath.Abs.pp access_path ;
        (* invariant 4: we should never map an access path to a trace consisting only of its
           corresponding footprint source. a trace like this is a waste of space, since we can
           lazily create it if/when someone actually tries to read the access path instead *)
        (* TODO: tmp to focus on invariant 1 *)
        if
          false
          && AccessPath.Abs.is_exact access_path
          && Sinks.is_empty sinks
          && Sources.Footprint.mem access_path footprint_sources
          && Sources.Footprint.exists
               (fun footprint_access_path (is_mem, _) ->
                 is_mem && AccessPath.Abs.equal access_path footprint_access_path )
               footprint_sources
        then
          Logging.die InternalError
            "The trace associated with %a consists only of its footprint source: %a"
            AccessPath.Abs.pp access_path pp trace )
      access_tree


  let make_summary {ProcData.pdesc; extras= {formal_map}} access_tree =
    let is_java = Typ.Procname.is_java (Procdesc.get_proc_name pdesc) in
    (* if a trace has footprint sources, attach them to the appropriate footprint var *)
    let access_tree' =
      TaintDomain.fold
        (fun access_tree_acc _ ((trace, _) as node) ->
          if TraceDomain.Sinks.is_empty (TraceDomain.sinks trace) then
            (* if this trace has no sinks, we don't need to attach it to anything *)
            access_tree_acc
          else
            TraceDomain.Sources.Footprint.fold
              (fun acc footprint_access_path (is_mem, _) ->
                if is_mem then
                  let node' =
                    match TaintDomain.get_node footprint_access_path acc with
                    | Some n ->
                        TaintDomain.node_join node n
                    | None ->
                        node
                  in
                  TaintDomain.add_node footprint_access_path node' acc
                else acc )
              (TraceDomain.sources trace).TraceDomain.Sources.footprint access_tree_acc )
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
      | TaintDomain.Subtree subtree ->
          TaintDomain.AccessMap.is_empty subtree
      | TaintDomain.Star ->
          true
    in
    (* replace formal names with footprint vars for their indices. For example, for `foo(o)`, we'll
       replace `o` with FP(1) *)
    let with_footprint_vars =
      AccessPath.BaseMap.fold
        (fun base ((trace, subtree) as node) acc ->
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
            | Some formal_index ->
                let base' = (Var.of_formal_index formal_index, snd base) in
                let joined_node =
                  try TaintDomain.node_join (AccessPath.BaseMap.find base' acc) node
                  with Caml.Not_found -> node
                in
                if is_empty_node joined_node then acc
                else AccessPath.BaseMap.add base' joined_node acc
            | None ->
                (* base is a local var *)
                acc )
        access_tree' TaintDomain.empty
    in
    if Config.developer_mode then check_invariants with_footprint_vars ;
    TaintSpecification.to_summary_access_tree with_footprint_vars


  let checker {Callbacks.tenv; summary; proc_desc} : Summary.t =
    (* bind parameters to a trace with a tainted source (if applicable) *)
    let make_initial pdesc =
      let pname = Procdesc.get_proc_name pdesc in
      List.fold
        ~f:(fun acc (name, typ, taint_opt) ->
          match taint_opt with
          | Some source ->
              let base_ap =
                AccessPath.Abs.Abstracted (AccessPath.of_pvar (Pvar.mk name pname) typ)
              in
              TaintDomain.add_trace base_ap (TraceDomain.of_source source) acc
          | None ->
              acc )
        ~init:TaintDomain.empty
        (TraceDomain.Source.get_tainted_formals pdesc tenv)
    in
    let initial = make_initial proc_desc in
    let extras =
      let formal_map = FormalMap.make proc_desc in
      {formal_map; summary}
    in
    let proc_data = ProcData.make proc_desc tenv extras in
    match Analyzer.compute_post proc_data ~initial with
    | Some access_tree ->
        Payload.update_summary (make_summary proc_data access_tree) summary
    | None ->
        if Procdesc.Node.get_succs (Procdesc.get_start_node proc_desc) <> [] then (
          L.internal_error "Couldn't compute post for %a. Broken CFG suspected" Typ.Procname.pp
            (Procdesc.get_proc_name proc_desc) ;
          summary )
        else summary
end
