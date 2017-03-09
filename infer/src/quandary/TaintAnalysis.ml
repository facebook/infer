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
      type summary = QuandarySummary.t

      let update_payload summary payload =
        { payload with Specs.quandary = Some summary; }

      let read_from_payload payload =
        match payload.Specs.quandary with
        | None -> Some (TaintSpecification.to_summary_access_tree TaintDomain.empty)
        | summary_opt -> summary_opt
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

  let make_footprint_var formal_index =
    Var.of_id (Ident.create_footprint Ident.name_spec formal_index)

  let make_footprint_access_path formal_index access_path =
    AccessPath.with_base_var (make_footprint_var formal_index) access_path

  module TransferFunctions (CFG : ProcCfg.S) = struct
    module CFG = CFG
    module Domain = Domain

    type extras = FormalMap.t

    let resolve_id id_map id =
      try Some (IdMapDomain.find id id_map)
      with Not_found -> None

    (* get the node associated with [access_path] in [access_tree] *)
    let access_path_get_node access_path access_tree (proc_data : FormalMap.t ProcData.t) loc =
      match TaintDomain.get_node access_path access_tree with
      | Some _ as node_opt ->
          node_opt
      | None ->
          let make_footprint_trace footprint_ap =
            let call_site =
              CallSite.make (Procdesc.get_proc_name proc_data.ProcData.pdesc) loc in
            let trace =
              TraceDomain.of_source
                (TraceDomain.Source.make_footprint footprint_ap call_site) in
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
    let access_path_get_trace access_path access_tree proc_data loc =
      match access_path_get_node access_path access_tree proc_data loc with
      | Some (trace, _) -> trace
      | None -> TraceDomain.empty

    (* get the node associated with [exp] in [access_tree] *)
    let exp_get_node ?(abstracted=false) exp typ { Domain.access_tree; id_map; } proc_data loc =
      let f_resolve_id = resolve_id id_map in
      match AccessPath.of_lhs_exp exp typ ~f_resolve_id with
      | Some raw_access_path ->
          let access_path =
            if abstracted
            then AccessPath.Abstracted raw_access_path
            else AccessPath.Exact raw_access_path in
          access_path_get_node access_path access_tree proc_data loc
      | None ->
          (* can't make an access path from [exp] *)
          None

    let analyze_assignment lhs_access_path rhs_exp rhs_typ astate proc_data loc =
      let rhs_node =
        match exp_get_node rhs_exp rhs_typ astate proc_data loc with
        | Some node -> node
        | None -> TaintDomain.empty_node in
      let access_tree = TaintDomain.add_node lhs_access_path rhs_node astate.Domain.access_tree in
      { astate with Domain.access_tree; }

    let analyze_id_assignment lhs_id rhs_exp rhs_typ ({ Domain.id_map; } as astate) =
      let f_resolve_id = resolve_id id_map in
      match AccessPath.of_lhs_exp rhs_exp rhs_typ ~f_resolve_id with
      | Some rhs_access_path ->
          let id_map' = IdMapDomain.add lhs_id rhs_access_path id_map in
          { astate with Domain.id_map = id_map'; }
      | None ->
          astate

    let add_source source ret_id ret_typ access_tree =
      let trace = TraceDomain.of_source source in
      let id_ap = AccessPath.Exact (AccessPath.of_id ret_id ret_typ) in
      TaintDomain.add_trace id_ap trace access_tree

    (** log any new reportable source-sink flows in [trace] *)
    let report_trace trace cur_site (proc_data : FormalMap.t ProcData.t) =
      let trace_of_pname pname =
        match Summary.read_summary proc_data.pdesc pname with
        | Some summary ->
            TaintDomain.fold
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
          "%a -> %a"
          TraceDomain.Source.pp original_source
          TraceDomain.Sink.pp final_sink in

      let report_error path =
        let caller_pname = Procdesc.get_proc_name proc_data.pdesc in
        let msg = Localise.to_string Localise.quandary_taint_error in
        let trace_str = F.asprintf "%a" pp_path_short path in
        let ltr = TraceDomain.to_loc_trace path in
        let exn = Exceptions.Checkers (msg, Localise.verbatim_desc trace_str) in
        Reporting.log_error caller_pname ~loc:(CallSite.loc cur_site) ~ltr exn in

      List.iter ~f:report_error (TraceDomain.get_reportable_paths ~cur_site trace ~trace_of_pname)

    let add_sinks sinks actuals ({ Domain.access_tree; id_map; } as astate) proc_data callee_site =
      let f_resolve_id = resolve_id id_map in
      (* add [sink] to the trace associated with the [formal_index]th actual *)
      let add_sink_to_actual access_tree_acc (sink_param : TraceDomain.Sink.parameter) =
        let actual_exp, actual_typ = List.nth_exn actuals sink_param.index in
        match AccessPath.of_lhs_exp actual_exp actual_typ ~f_resolve_id with
        | Some actual_ap_raw ->
            let actual_ap =
              let is_array_typ = match actual_typ with
                | Typ.Tptr (Tarray _, _) (* T* [] (Java-style) *)
                | Tptr (Tptr _, _) (* T** (C/C++ style 1) *)
                | Tarray _ (* T[] C/C++ style 2 *) ->
                    true
                | _ ->
                    false in
              (* conisder any sources that are reachable from an array *)
              if sink_param.report_reachable || is_array_typ
              then AccessPath.Abstracted actual_ap_raw
              else AccessPath.Exact actual_ap_raw in
            begin
              match access_path_get_node
                      actual_ap access_tree_acc proc_data (CallSite.loc callee_site) with
              | Some (actual_trace, _) ->
                  let actual_trace' = TraceDomain.add_sink sink_param.sink actual_trace in
                  report_trace actual_trace' callee_site proc_data;
                  TaintDomain.add_trace actual_ap actual_trace' access_tree_acc
              | None ->
                  access_tree_acc
            end
        | None ->
            access_tree_acc in
      let access_tree' = List.fold ~f:add_sink_to_actual ~init:access_tree sinks in
      { astate with Domain.access_tree = access_tree'; }

    let apply_summary
        ret_opt
        actuals
        summary
        (astate_in : Domain.astate)
        (proc_data : FormalMap.t ProcData.t)
        callee_site =
      let callee_loc = CallSite.loc callee_site in
      let caller_access_tree = astate_in.access_tree in

      let get_caller_ap formal_ap =
        let apply_return ret_ap = match ret_opt with
          | Some (ret_id, _) -> AccessPath.with_base_var (Var.of_id ret_id) ret_ap
          | None -> failwith "Have summary for retval, but no ret id to bind it to!" in
        let get_actual_ap formal_index =
          let f_resolve_id = resolve_id astate_in.id_map in
          List.nth actuals formal_index |>
          Option.value_map
            ~f:(fun (actual_exp, actual_typ) ->
                AccessPath.of_lhs_exp actual_exp actual_typ ~f_resolve_id )
            ~default:None in
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

      let get_caller_ap_node ap access_tree =
        match get_caller_ap ap with
        | Some caller_ap ->
            let caller_node_opt =
              access_path_get_node caller_ap access_tree proc_data callee_loc in
            let caller_node = match caller_node_opt with
              | Some caller_node -> caller_node | None -> TaintDomain.empty_node in
            caller_ap, caller_node
        | None ->
            ap, TaintDomain.empty_node in

      let replace_footprint_sources callee_trace caller_trace access_tree =
        let replace_footprint_source source acc =
          match TraceDomain.Source.get_footprint_access_path source with
          | Some footprint_access_path ->
              let _, (caller_ap_trace, _) = get_caller_ap_node footprint_access_path access_tree in
              TraceDomain.join caller_ap_trace acc
          | None ->
              acc in
        TraceDomain.Sources.fold
          replace_footprint_source (TraceDomain.sources callee_trace) caller_trace in

      let add_to_caller_tree access_tree_acc callee_ap callee_trace =
        let caller_ap, (caller_trace, caller_tree) = get_caller_ap_node callee_ap access_tree_acc in
        let caller_trace' = replace_footprint_sources callee_trace caller_trace access_tree_acc in
        let appended_trace =
          TraceDomain.append caller_trace' callee_trace callee_site in
        report_trace appended_trace callee_site proc_data;
        TaintDomain.add_node caller_ap (appended_trace, caller_tree) access_tree_acc in

      let access_tree =
        TaintDomain.fold
          add_to_caller_tree
          (TaintSpecification.of_summary_access_tree summary)
          caller_access_tree in
      { astate_in with access_tree; }

    let exec_instr (astate : Domain.astate) (proc_data : FormalMap.t ProcData.t) _ instr =
      let f_resolve_id = resolve_id astate.id_map in
      match instr with
      | Sil.Load (lhs_id, rhs_exp, rhs_typ, _) ->
          analyze_id_assignment (Var.of_id lhs_id) rhs_exp rhs_typ astate
      | Sil.Store (Exp.Lvar lhs_pvar, lhs_typ, rhs_exp, _) when Pvar.is_frontend_tmp lhs_pvar ->
          analyze_id_assignment (Var.of_pvar lhs_pvar) rhs_exp lhs_typ astate
      | Sil.Store (Exp.Lvar lhs_pvar, _, Exp.Exn _, _) when Pvar.is_return lhs_pvar ->
          (* the Java frontend translates `throw Exception` as `return Exception`, which is a bit
             wonky. this translation causes problems for us in computing a summary when an
             exception is "returned" from a void function. skip code like this for now
             (fix via t14159157 later *)
          astate
      | Sil.Store (Exp.Lvar lhs_pvar, _, rhs_exp, _)
        when Pvar.is_return lhs_pvar && Exp.is_null_literal rhs_exp &&
             Typ.equal Tvoid (Procdesc.get_ret_type proc_data.pdesc) ->
          (* similar to the case above; the Java frontend translates "return no exception" as
             `return null` in a void function *)
          astate
      | Sil.Store (lhs_exp, lhs_typ, rhs_exp, loc) ->
          let lhs_access_path =
            match AccessPath.of_lhs_exp lhs_exp lhs_typ ~f_resolve_id with
            | Some access_path ->
                access_path
            | None ->
                failwithf
                  "Assignment to unexpected lhs expression %a in proc %a at loc %a"
                  Exp.pp lhs_exp
                  Typ.Procname.pp (Procdesc.get_proc_name (proc_data.pdesc))
                  Location.pp loc in
          let astate' =
            analyze_assignment
              (AccessPath.Exact lhs_access_path) rhs_exp lhs_typ astate proc_data loc in
          begin
            (* direct `exp = id` assignments are treated specially; we update the id map too. this
               is so future reads of `exp` will get the subtree associated with `id` (needed to
               handle the `id = foo(); exp = id case` and similar). *)
            match rhs_exp with
            | Exp.Var rhs_id ->
                let existing_accesses =
                  try snd (IdMapDomain.find (Var.of_id rhs_id) astate'.Domain.id_map)
                  with Not_found -> [] in
                let lhs_ap' = AccessPath.append lhs_access_path existing_accesses in
                let id_map' = IdMapDomain.add (Var.of_id rhs_id) lhs_ap' astate'.Domain.id_map in
                { astate' with Domain.id_map = id_map'; }
            | _ ->
                astate'
          end
      | Sil.Call (Some (ret_id, _), Const (Cfun callee_pname), args, loc, _)
        when BuiltinDecl.is_declared callee_pname ->
          if Typ.Procname.equal callee_pname BuiltinDecl.__cast
          then
            match args with
            | (cast_target, cast_typ) :: _ ->
                analyze_id_assignment (Var.of_id ret_id) cast_target cast_typ astate
            | _ ->
                failwithf
                  "Unexpected cast %a in procedure %a at line %a"
                  (Sil.pp_instr Pp.text) instr
                  Typ.Procname.pp (Procdesc.get_proc_name (proc_data.pdesc))
                  Location.pp loc
          else
            astate

      | Sil.Call (ret, Const (Cfun called_pname), actuals, callee_loc, call_flags) ->

          let handle_unknown_call callee_pname astate =
            let exp_join_traces trace_acc (exp, typ) =
              match exp_get_node ~abstracted:true exp typ astate proc_data callee_loc with
              | Some (trace, _) -> TraceDomain.join trace trace_acc
              | None -> trace_acc in
            let propagate_to_access_path access_path actuals (astate : Domain.astate) =
              let initial_trace =
                access_path_get_trace access_path astate.access_tree proc_data callee_loc in
              let trace_with_propagation =
                List.fold ~f:exp_join_traces ~init:initial_trace actuals in
              let access_tree =
                TaintDomain.add_trace access_path trace_with_propagation astate.access_tree in
              { astate with access_tree; } in
            let handle_unknown_call_ astate_acc propagation =
              match propagation, actuals, ret with
              | _, [], _ ->
                  astate_acc
              | TaintSpec.Propagate_to_return, actuals, Some (ret_id, ret_typ) ->
                  let ret_ap = AccessPath.Exact (AccessPath.of_id ret_id ret_typ) in
                  propagate_to_access_path ret_ap actuals astate_acc
              | TaintSpec.Propagate_to_receiver,
                (receiver_exp, receiver_typ) :: (_ :: _ as other_actuals),
                _ ->
                  begin
                    match AccessPath.of_lhs_exp receiver_exp receiver_typ ~f_resolve_id with
                    | Some ap ->
                        propagate_to_access_path (AccessPath.Exact ap) other_actuals astate_acc
                    | None ->
                        (* this can happen when (for example) the receiver is a string literal *)
                        astate_acc
                  end
              | _ ->
                  astate_acc in

            let propagations =
              TaintSpecification.handle_unknown_call
                callee_pname
                (Option.map ~f:snd ret)
                actuals
                proc_data.tenv in
            List.fold ~f:handle_unknown_call_ ~init:astate propagations in

          let analyze_call astate_acc callee_pname =
            let call_site = CallSite.make callee_pname callee_loc in

            let sinks = TraceDomain.Sink.get call_site actuals proc_data.ProcData.tenv in
            let astate_with_sink = match sinks with
              | [] -> astate
              | sinks -> add_sinks sinks actuals astate proc_data call_site in

            let source = TraceDomain.Source.get call_site proc_data.tenv in
            let astate_with_source =
              match source, ret with
              | Some source, Some (ret_id, ret_typ) ->
                  let access_tree = add_source source ret_id ret_typ astate_with_sink.access_tree in
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
                    apply_summary ret actuals summary astate_with_source proc_data call_site
                | None ->
                    handle_unknown_call callee_pname astate_with_source in

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
                L.out "Skipping highly polymorphic call site for %a@." Typ.Procname.pp called_pname;
                [called_pname]
              end in
          (* for each possible target of the call, apply the summary. join all results together *)
          List.fold ~f:analyze_call ~init:Domain.empty targets
      | Sil.Call _ ->
          failwith "Unimp: non-pname call expressions"
      | Sil.Nullify (pvar, _) ->
          let id_map = IdMapDomain.remove (Var.of_pvar pvar) astate.id_map in
          { astate with id_map; }
      | Sil.Remove_temps (ids, _) ->
          let id_map =
            List.fold
              ~f:(fun acc id -> IdMapDomain.remove (Var.of_id id) acc)
              ~init:astate.id_map
              ids in
          { astate with id_map; }
      | Sil.Prune _ | Abstract _ | Declare_locals _ ->
          astate
  end

  module Analyzer = AbstractInterpreter.Make (ProcCfg.Exceptional) (TransferFunctions)

  let make_summary formal_map access_tree =
    let access_tree' =
      AccessPath.BaseMap.fold
        (fun base node acc ->
           let base' =
             match FormalMap.get_formal_index base formal_map with
             | Some formal_index -> make_footprint_var formal_index, snd base
             | None -> base in
           AccessPath.BaseMap.add base' node acc)
        access_tree
        TaintDomain.empty in

    TaintSpecification.to_summary_access_tree access_tree'

  module Interprocedural = AbstractInterpreter.Interprocedural(Summary)

  let checker ({ Callbacks.tenv } as callback) =

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
          Preanal.do_dynamic_dispatch proc_data.pdesc (Cg.create None) proc_data.tenv;
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
    ignore(Interprocedural.compute_and_store_post ~compute_post ~make_extras callback)
end
