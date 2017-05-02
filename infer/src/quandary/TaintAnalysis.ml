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
  module IdMapDomain = IdAccessPathMapDomain

  module Summary = Summary.Make(struct
      type payload = QuandarySummary.t

      let update_payload quandary_payload (summary : Specs.summary) =
        { summary with payload = { summary.payload with quandary = Some quandary_payload }}

      let read_payload (summary : Specs.summary) =
        summary.payload.quandary
    end)

  module Domain = struct
    type astate =
      {
        access_tree : TaintDomain.astate; (* mapping of access paths to trace sets *)
        id_map : IdMapDomain.astate; (* mapping of id's to access paths for normalization *)
      }

    let empty =
      let access_tree = TaintDomain.empty in
      let id_map = IdMapDomain.empty in
      { access_tree; id_map; }

    let (<=) ~lhs ~rhs =
      if phys_equal lhs rhs
      then true
      else
        TaintDomain.(<=) ~lhs:lhs.access_tree ~rhs:rhs.access_tree &&
        IdMapDomain.(<=) ~lhs:lhs.id_map ~rhs:rhs.id_map

    let join astate1 astate2 =
      if phys_equal astate1 astate2
      then astate1
      else
        let access_tree = TaintDomain.join astate1.access_tree astate2.access_tree in
        let id_map = IdMapDomain.join astate1.id_map astate2.id_map in
        { access_tree; id_map; }

    let widen ~prev ~next ~num_iters =
      if phys_equal prev next
      then prev
      else
        let access_tree =
          TaintDomain.widen ~prev:prev.access_tree ~next:next.access_tree ~num_iters in
        let id_map = IdMapDomain.widen ~prev:prev.id_map ~next:next.id_map ~num_iters in
        { access_tree; id_map; }

    let pp fmt { access_tree; id_map; } =
      F.fprintf fmt "(%a, %a)" TaintDomain.pp access_tree IdMapDomain.pp id_map
  end

  let is_global (var, _) = match var with
    | Var.ProgramVar pvar -> Pvar.is_global pvar
    | Var.LogicalVar _ -> false

  let is_return (var, _) = match var with
    | Var.ProgramVar pvar -> Pvar.is_return pvar
    | Var.LogicalVar _ -> false

  let is_footprint (var, _) = match var with
    | Var.ProgramVar _ -> false
    | Var.LogicalVar id -> Ident.is_footprint id

  let make_footprint_var formal_index =
    Var.of_id (Ident.create_footprint Ident.name_spec formal_index)

  let make_footprint_access_path formal_index access_path =
    let _, base_typ = fst (AccessPath.extract access_path) in
    AccessPath.with_base (make_footprint_var formal_index, base_typ) access_path

  module TransferFunctions (CFG : ProcCfg.S) = struct
    module CFG = CFG
    module Domain = Domain

    type extras = FormalMap.t

    let resolve_id id_map id =
      try Some (IdMapDomain.find id id_map)
      with Not_found -> None

    (* get the node associated with [access_path] in [access_tree] *)
    let access_path_get_node access_path access_tree (proc_data : FormalMap.t ProcData.t) =
      match TaintDomain.get_node access_path access_tree with
      | Some _ as node_opt ->
          node_opt
      | None ->
          let make_footprint_trace footprint_ap =
            let trace =
              TraceDomain.of_source
                (TraceDomain.Source.make_footprint footprint_ap proc_data.pdesc) in
            Some (TaintDomain.make_normal_leaf trace) in
          let root, _ = AccessPath.extract access_path in
          match FormalMap.get_formal_index root proc_data.extras with
          | Some formal_index ->
              make_footprint_trace (make_footprint_access_path formal_index access_path)
          | None ->
              if is_global root
              then make_footprint_trace access_path
              else None

    (* get the trace associated with [access_path] in [access_tree]. *)
    let access_path_get_trace access_path access_tree proc_data =
      match access_path_get_node access_path access_tree proc_data with
      | Some (trace, _) -> trace
      | None -> TraceDomain.empty

    let exp_get_node_ ~abstracted raw_access_path access_tree proc_data =
      let access_path =
        if abstracted
        then AccessPath.Abstracted raw_access_path
        else AccessPath.Exact raw_access_path in
      access_path_get_node access_path access_tree proc_data

    (* get the node associated with [exp] in [access_tree] *)
    let exp_get_node ?(abstracted=false) exp typ { Domain.access_tree; id_map; } proc_data =
      let f_resolve_id = resolve_id id_map in
      match AccessPath.of_lhs_exp exp typ ~f_resolve_id with
      | Some raw_access_path -> exp_get_node_ ~abstracted raw_access_path access_tree proc_data
      | None -> None

    (* get the node associated with [exp] in [access_tree] *)
    let hil_exp_get_node ?(abstracted=false) (exp : HilExp.t) access_tree proc_data =
      match exp with
      | AccessPath access_path -> exp_get_node_ ~abstracted access_path access_tree proc_data
      | _ -> None

    let add_source source ret_base access_tree =
      let trace = TraceDomain.of_source source in
      let id_ap = AccessPath.Exact (ret_base, []) in
      TaintDomain.add_trace id_ap trace access_tree

    let endpoints = String.Set.of_list (QuandaryConfig.Endpoint.of_json Config.quandary_endpoints)

    let is_endpoint source =
      match CallSite.pname (TraceDomain.Source.call_site source) with
      | Typ.Procname.Java java_pname ->
          String.Set.mem endpoints (Typ.Procname.java_get_class_name java_pname)
      | _ ->
          false

    (** log any new reportable source-sink flows in [trace] *)
    let report_trace trace cur_site (proc_data : FormalMap.t ProcData.t) =
      let trace_of_pname pname =
        if Typ.Procname.equal pname (Procdesc.get_proc_name proc_data.pdesc)
        then
          (* read_summary will trigger ondemand analysis of the current proc. we don't want that. *)
          TraceDomain.empty
        else
          match Summary.read_summary proc_data.pdesc pname with
          | Some summary ->
              TaintDomain.trace_fold
                (fun acc _ trace -> TraceDomain.join trace acc)
                (TaintSpecification.of_summary_access_tree summary)
                TraceDomain.empty
          | None ->
              TraceDomain.empty in

      let pp_path_short fmt (_, sources_passthroughs, sinks_passthroughs) =
        let original_source = fst (List.hd_exn sources_passthroughs) in
        let final_sink = fst (List.hd_exn sinks_passthroughs) in
        F.fprintf
          fmt
          "%a -> %a%s"
          TraceDomain.Source.pp original_source
          TraceDomain.Sink.pp final_sink
          (if is_endpoint original_source then ". Note: source is an endpoint." else "") in

      let report_error path =
        let caller_pname = Procdesc.get_proc_name proc_data.pdesc in
        let msg = Localise.to_issue_id Localise.quandary_taint_error in
        let trace_str = F.asprintf "%a" pp_path_short path in
        let ltr = TraceDomain.to_loc_trace path in
        let exn = Exceptions.Checkers (msg, Localise.verbatim_desc trace_str) in
        Reporting.log_error caller_pname ~loc:(CallSite.loc cur_site) ~ltr exn in

      List.iter ~f:report_error (TraceDomain.get_reportable_paths ~cur_site trace ~trace_of_pname)

    let add_sinks sinks actuals ({ Domain.access_tree; } as astate) proc_data callee_site =
      (* add [sink] to the trace associated with the [formal_index]th actual *)
      let add_sink_to_actual access_tree_acc (sink_param : TraceDomain.Sink.parameter) =
        match List.nth_exn actuals sink_param.index with
        | HilExp.AccessPath actual_ap_raw ->
            let actual_ap =
              let is_array_typ =
                match AccessPath.Raw.get_typ actual_ap_raw proc_data.ProcData.tenv with
                | Some
                    ({ desc=(
                         Typ.Tptr ({desc=Tarray _}, _) (* T* [] (Java-style) *)
                       | Tptr ({desc=Tptr _}, _) (* T** (C/C++ style 1) *)
                       | Tarray _ )}) (* T[] C/C++ style 2 *) ->
                    true
                | _ ->
                    false in
              (* conisder any sources that are reachable from an array *)
              if sink_param.report_reachable || is_array_typ
              then AccessPath.Abstracted actual_ap_raw
              else AccessPath.Exact actual_ap_raw in
            begin
              match access_path_get_node actual_ap access_tree_acc proc_data with
              | Some (actual_trace, _) ->
                  let actual_trace' = TraceDomain.add_sink sink_param.sink actual_trace in
                  report_trace actual_trace' callee_site proc_data;
                  TaintDomain.add_trace actual_ap actual_trace' access_tree_acc
              | None ->
                  access_tree_acc
            end
        | _ ->
            access_tree_acc in
      let access_tree' = List.fold ~f:add_sink_to_actual ~init:access_tree sinks in
      { astate with Domain.access_tree = access_tree'; }

    let apply_summary
        ret_opt
        (actuals : HilExp.t list)
        summary
        (astate_in : Domain.astate)
        (proc_data : FormalMap.t ProcData.t)
        callee_site =
      let caller_access_tree = astate_in.access_tree in

      let get_caller_ap formal_ap =
        let apply_return ret_ap = match ret_opt with
          | Some base_var -> AccessPath.with_base base_var ret_ap
          | None -> failwith "Have summary for retval, but no ret id to bind it to!" in
        let get_actual_ap formal_index =
          Option.value_map
            ~f:(function
                | HilExp.AccessPath access_path -> Some access_path
                | _ -> None)
            ~default:None
            (List.nth actuals formal_index) in
        let project ~formal_ap ~actual_ap =
          let projected_ap = AccessPath.append actual_ap (snd (AccessPath.extract formal_ap)) in
          if AccessPath.is_exact formal_ap
          then AccessPath.Exact projected_ap
          else AccessPath.Abstracted projected_ap in
        let base_var, _ = fst (AccessPath.extract formal_ap) in
        match base_var with
        | Var.ProgramVar pvar ->
            if Pvar.is_return pvar
            then Some (apply_return formal_ap)
            else Some formal_ap
        | Var.LogicalVar id when Ident.is_footprint id ->
            begin
              (* summaries store the index of the formal parameter in the ident stamp *)
              match get_actual_ap (Ident.get_stamp id) with
              | Some actual_ap ->
                  let projected_ap = project ~formal_ap ~actual_ap in
                  Some projected_ap
              | None ->
                  None
            end
        | _ ->
            None in

      let get_caller_ap_node_opt ap access_tree =
        let get_caller_node caller_ap =
          let caller_node_opt = access_path_get_node caller_ap access_tree proc_data in
          let caller_node = Option.value ~default:TaintDomain.empty_node caller_node_opt in
          caller_ap, caller_node in
        Option.map (get_caller_ap ap) ~f:get_caller_node in

      let replace_footprint_sources callee_trace caller_trace access_tree =
        let replace_footprint_source source acc =
          match TraceDomain.Source.get_footprint_access_path source with
          | Some footprint_access_path ->
              begin
                match get_caller_ap_node_opt footprint_access_path access_tree with
                | Some (_, (caller_ap_trace, _)) -> TraceDomain.join caller_ap_trace acc
                | None -> acc
              end
          | None ->
              acc in
        TraceDomain.Sources.fold
          replace_footprint_source (TraceDomain.sources callee_trace) caller_trace in

      let instantiate_and_report callee_trace caller_trace access_tree =
        let caller_trace' = replace_footprint_sources callee_trace caller_trace access_tree in
        let appended_trace = TraceDomain.append caller_trace' callee_trace callee_site in
        report_trace appended_trace callee_site proc_data;
        appended_trace in

      let add_to_caller_tree access_tree_acc callee_ap callee_trace =
        match get_caller_ap_node_opt callee_ap access_tree_acc with
        | Some (caller_ap, (caller_trace, caller_tree)) ->
            let trace = instantiate_and_report callee_trace caller_trace access_tree_acc in
            TaintDomain.add_node caller_ap (trace, caller_tree) access_tree_acc
        | None ->
            ignore (instantiate_and_report callee_trace TraceDomain.empty access_tree_acc);
            access_tree_acc in

      let access_tree =
        TaintDomain.trace_fold
          add_to_caller_tree
          (TaintSpecification.of_summary_access_tree summary)
          caller_access_tree in
      { astate_in with access_tree; }

    let exec_hil_instr (astate : Domain.astate) (proc_data : FormalMap.t ProcData.t) instr =
      let exec_instr_ (instr : HilInstr.t) = match instr with
        | Write (((Var.ProgramVar pvar, _), []), HilExp.Exception _, _) when Pvar.is_return pvar ->
            (* the Java frontend translates `throw Exception` as `return Exception`, which is a bit
               wonky. this translation causes problems for us in computing a summary when an
               exception is "returned" from a void function. skip code like this for now, fix via
               t14159157 later *)
            astate

        | Write (((Var.ProgramVar pvar, _), []), rhs_exp, _)
          when Pvar.is_return pvar && HilExp.is_null_literal rhs_exp &&
               Typ.equal_desc Tvoid (Procdesc.get_ret_type proc_data.pdesc).desc ->
            (* similar to the case above; the Java frontend translates "return no exception" as
               `return null` in a void function *)
            astate

        | Write (lhs_access_path, rhs_exp, _) ->
            let access_tree =
              let rhs_node =
                Option.value
                  (hil_exp_get_node rhs_exp astate.access_tree proc_data)
                  ~default:TaintDomain.empty_node in
              TaintDomain.add_node (AccessPath.Exact lhs_access_path) rhs_node astate.access_tree in
            { astate with access_tree; }

        | Call (ret_opt, Direct called_pname, actuals, call_flags, callee_loc) ->
            let handle_unknown_call callee_pname access_tree =
              let is_variadic = match callee_pname with
                | Typ.Procname.Java pname ->
                    begin
                      match List.rev (Typ.Procname.java_get_parameters pname) with
                      | (_, "java.lang.Object[]") :: _ -> true
                      | _ -> false
                    end
                | _ -> false in
              let should_taint_typ typ = is_variadic || TaintSpecification.is_taintable_type typ in
              let exp_join_traces trace_acc exp =
                match hil_exp_get_node ~abstracted:true exp access_tree proc_data with
                | Some (trace, _) -> TraceDomain.join trace trace_acc
                | None -> trace_acc in
              let propagate_to_access_path access_path actuals access_tree =
                let initial_trace =
                  access_path_get_trace access_path access_tree proc_data in
                let trace_with_propagation =
                  List.fold ~f:exp_join_traces ~init:initial_trace actuals in
                let filtered_sources =
                  TraceDomain.Sources.filter (fun source ->
                      match TraceDomain.Source.get_footprint_access_path source with
                      | Some access_path ->
                          Option.exists
                            (AccessPath.Raw.get_typ (AccessPath.extract access_path) proc_data.tenv)
                            ~f:should_taint_typ
                      | None ->
                          true)
                    (TraceDomain.sources trace_with_propagation) in
                if TraceDomain.Sources.is_empty filtered_sources
                then
                  access_tree
                else
                  let trace' = TraceDomain.update_sources trace_with_propagation filtered_sources in
                  TaintDomain.add_trace access_path trace' access_tree in
              let handle_unknown_call_ astate_acc propagation =
                match propagation, actuals, ret_opt with
                | _, [], _ ->
                    astate_acc
                | TaintSpec.Propagate_to_return, actuals, Some ret_ap ->
                    propagate_to_access_path (AccessPath.Exact (ret_ap, [])) actuals astate_acc
                | TaintSpec.Propagate_to_receiver,
                  AccessPath receiver_ap :: (_ :: _ as other_actuals),
                  _ ->
                    propagate_to_access_path (AccessPath.Exact receiver_ap) other_actuals astate_acc
                | _ ->
                    astate_acc in

              let propagations =
                TaintSpecification.handle_unknown_call
                  callee_pname
                  (Option.map ~f:snd ret_opt)
                  actuals
                  proc_data.tenv in
              List.fold ~f:handle_unknown_call_ ~init:access_tree propagations in

            let analyze_call astate_acc callee_pname =
              let call_site = CallSite.make callee_pname callee_loc in

              let sinks = TraceDomain.Sink.get call_site actuals proc_data.ProcData.tenv in
              let astate_with_sink = match sinks with
                | [] -> astate
                | sinks -> add_sinks sinks actuals astate proc_data call_site in

              let source = TraceDomain.Source.get call_site proc_data.tenv in
              let astate_with_source =
                match source, ret_opt with
                | Some source, Some ret_exp ->
                    let access_tree = add_source source ret_exp astate_with_sink.access_tree in
                    { astate_with_sink with access_tree; }
                | Some _, None ->
                    L.err
                      "Warning: %a is marked as a source, but has no return value"
                      Typ.Procname.pp callee_pname;
                    astate_with_sink
                | None, _ ->
                    astate_with_sink in

              let astate_with_summary =
                if sinks <> [] || Option.is_some source
                then
                  (* don't use a summary for a procedure that is a direct source or sink *)
                  astate_with_source
                else
                  match Summary.read_summary proc_data.pdesc callee_pname with
                  | Some summary ->
                      apply_summary ret_opt actuals summary astate_with_source proc_data call_site
                  | None ->
                      let access_tree =
                        handle_unknown_call callee_pname astate_with_source.access_tree in
                      { astate with access_tree; } in
              Domain.join astate_acc astate_with_summary in

            (* highly polymorphic call sites stress reactive mode too much by using too much memory.
               here, we choose an arbitrary call limit that allows us to finish the analysis in
               practice. this is obviously unsound; will try to remove in the future. *)
            let max_calls = 3 in
            let targets =
              if List.length call_flags.cf_targets <= max_calls
              then
                called_pname :: call_flags.cf_targets
              else
                begin
                  L.out
                    "Skipping highly polymorphic call site for %a@." Typ.Procname.pp called_pname;
                  [called_pname]
                end in
            (* for each possible target of the call, apply the summary. join all results together *)
            List.fold ~f:analyze_call ~init:Domain.empty targets
        | _ ->
            astate in

      let f_resolve_id id =
        try Some (IdAccessPathMapDomain.find id astate.id_map)
        with Not_found -> None in
      match HilInstr.of_sil ~f_resolve_id instr with
      | Bind (id, access_path) ->
          let id_map = IdAccessPathMapDomain.add id access_path astate.id_map in
          { astate with id_map; }
      | Unbind ids ->
          let id_map =
            List.fold
              ~f:(fun acc id -> IdAccessPathMapDomain.remove id acc) ~init:astate.id_map ids in
          { astate with id_map; }
      | Instr hil_instr ->
          exec_instr_ hil_instr
      | Ignore ->
          astate

    let exec_instr (astate : Domain.astate) (proc_data : FormalMap.t ProcData.t) _ instr =
      exec_hil_instr astate proc_data instr
  end

  module Analyzer = AbstractInterpreter.Make (ProcCfg.Exceptional) (TransferFunctions)

  let make_summary formal_map access_tree =
    (* if a trace has footprint sources, attach them to the appropriate footprint var *)
    let access_tree' =
      TaintDomain.fold
        (fun access_tree_acc _ ((trace, _) as node) ->
           if TraceDomain.Sinks.is_empty (TraceDomain.sinks trace)
           then
             (* if this trace has no sinks, we don't need to attach it to anything *)
             access_tree_acc
           else
             TraceDomain.Sources.fold
               (fun source acc ->
                  match TraceDomain.Source.get_footprint_access_path source with
                  | Some footprint_access_path ->
                      let node' =
                        match TaintDomain.get_node footprint_access_path acc with
                        | Some n -> TaintDomain.node_join node n
                        | None -> node in
                      TaintDomain.add_node footprint_access_path node' acc
                  | None ->
                      acc)
               (TraceDomain.sources trace)
               access_tree_acc)
        access_tree
        access_tree in

    (* should only be used on nodes associated with a footprint base *)
    let is_empty_node (trace, tree) =
      TraceDomain.Sinks.is_empty (TraceDomain.sinks trace) &&
      match tree with
      | TaintDomain.Subtree subtree -> TaintDomain.AccessMap.is_empty subtree
      | TaintDomain.Star -> true in

    (* replace formal names with footprint vars for their indices. For example, for `foo(o)`, we'll
       replace `o` with FP(1) *)
    let with_footprint_vars =
      AccessPath.BaseMap.fold
        (fun base ((trace, subtree) as node) acc ->
           if is_global base || is_return base
           then AccessPath.BaseMap.add base node acc
           else if is_footprint base
           then
             if is_empty_node node
             then
               acc
             else
               let node' =
                 if TraceDomain.Sinks.is_empty (TraceDomain.sinks trace)
                 then TraceDomain.empty, subtree
                 else node in
               AccessPath.BaseMap.add base node' acc
           else
             match FormalMap.get_formal_index base formal_map with
             | Some formal_index ->
                 let base' = make_footprint_var formal_index, snd base in
                 let joined_node =
                   try TaintDomain.node_join (AccessPath.BaseMap.find base' acc) node
                   with Not_found -> node in
                 if is_empty_node joined_node
                 then
                   acc
                 else
                   AccessPath.BaseMap.add base' joined_node acc
             | None ->
                 (* base is a local var *)
                 acc)
        access_tree'
        TaintDomain.empty in

    TaintSpecification.to_summary_access_tree with_footprint_vars

  module Interprocedural = AbstractInterpreter.Interprocedural(Summary)

  let checker ({ Callbacks.tenv; } as callback) : Specs.summary =

    (* bind parameters to a trace with a tainted source (if applicable) *)
    let make_initial pdesc =
      let pname = Procdesc.get_proc_name pdesc in
      let access_tree =
        List.fold ~f:(fun acc (name, typ, taint_opt) ->
            match taint_opt with
            | Some source ->
                let base_ap = AccessPath.Exact (AccessPath.of_pvar (Pvar.mk name pname) typ) in
                TaintDomain.add_trace base_ap (TraceDomain.of_source source) acc
            | None ->
                acc)
          ~init:TaintDomain.empty
          (TraceDomain.Source.get_tainted_formals pdesc tenv) in
      if TaintDomain.BaseMap.is_empty access_tree
      then Domain.empty
      else { Domain.empty with Domain.access_tree; } in

    let compute_post (proc_data : FormalMap.t ProcData.t) =
      if not (Procdesc.did_preanalysis proc_data.pdesc)
      then
        begin
          Preanal.do_liveness proc_data.pdesc proc_data.tenv;
          Preanal.do_dynamic_dispatch proc_data.pdesc (Cg.create SourceFile.invalid) proc_data.tenv;
        end;
      let initial = make_initial proc_data.pdesc in
      match Analyzer.compute_post proc_data ~initial with
      | Some { access_tree; } ->
          Some (make_summary proc_data.extras access_tree)
      | None ->
          if Procdesc.Node.get_succs (Procdesc.get_start_node proc_data.pdesc) <> []
          then failwith "Couldn't compute post"
          else None in
    let make_extras = FormalMap.make in
    Interprocedural.compute_and_store_post ~compute_post ~make_extras callback
end
