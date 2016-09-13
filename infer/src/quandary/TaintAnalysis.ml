(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module F = Format
module L = Logging

(** Create a taint analysis from a trace domain *)

module Summary = Summary.Make(struct
    type summary = QuandarySummary.t

    let update_payload summary payload =
      { payload with Specs.quandary = Some summary; }

    let read_from_payload payload =
      match payload.Specs.quandary with
      | Some summary -> summary
      | None -> failwith "Failed to load summary"
  end)


module Make (TraceDomain : QuandarySummary.Trace) = struct

  module TaintDomain = AccessTree.Make (TraceDomain)
  module IdMapDomain = IdAccessPathMapDomain

  module Domain = struct
    type astate =
      {
        access_tree : TaintDomain.astate; (* mapping of access paths to trace sets *)
        id_map : IdMapDomain.astate; (* mapping of id's to access paths for normalization *)
      }

    let initial =
      let access_tree = TaintDomain.initial in
      let id_map = IdMapDomain.initial in
      { access_tree; id_map; }

    let (<=) ~lhs ~rhs =
      if lhs == rhs
      then true
      else
        TaintDomain.(<=) ~lhs:lhs.access_tree ~rhs:rhs.access_tree &&
        IdMapDomain.(<=) ~lhs:lhs.id_map ~rhs:rhs.id_map

    let join astate1 astate2 =
      if astate1 == astate2
      then astate1
      else
        let access_tree = TaintDomain.join astate1.access_tree astate2.access_tree in
        let id_map = IdMapDomain.join astate1.id_map astate2.id_map in
        { access_tree; id_map; }

    let widen ~prev ~next ~num_iters =
      if prev == next
      then prev
      else
        let access_tree =
          TaintDomain.widen ~prev:prev.access_tree ~next:next.access_tree ~num_iters in
        let id_map = IdMapDomain.widen ~prev:prev.id_map ~next:next.id_map ~num_iters in
        { access_tree; id_map; }

    let pp fmt { access_tree; id_map; } =
      F.fprintf fmt "(%a, %a)" TaintDomain.pp access_tree IdMapDomain.pp id_map
  end

  module TransferFunctions (CFG : ProcCfg.S) = struct
    module CFG = CFG
    module Domain = Domain

    type formal_list = AccessPath.base list
    type extras = formal_list

    let is_formal base proc_data =
      IList.exists (AccessPath.base_equal base) proc_data.ProcData.extras

    let is_rooted_in_formal ap proc_data =
      let root, _ = AccessPath.extract ap in
      is_formal root proc_data

    let resolve_id id_map id =
      try Some (IdMapDomain.find id id_map)
      with Not_found -> None

    (* get the node associated with [access_path] in [access_tree] *)
    let access_path_get_node access_path access_tree proc_data loc =
      match TaintDomain.get_node access_path access_tree with
      | Some _ as node_opt ->
          node_opt
      | None when is_rooted_in_formal access_path proc_data ->
          let call_site = CallSite.make (Cfg.Procdesc.get_proc_name proc_data.ProcData.pdesc) loc in
          let trace =
            TraceDomain.of_source (TraceDomain.Source.make_footprint access_path call_site) in
          Some (TaintDomain.make_normal_leaf trace)
      | None ->
          None

    (* get the node associated with [exp] in [access_tree] *)
    let exp_get_node exp typ { Domain.access_tree; id_map; } proc_data loc =
      let f_resolve_id = resolve_id id_map in
      match AccessPath.of_exp exp typ ~f_resolve_id with
      | Some access_path ->
          access_path_get_node (AccessPath.Exact access_path) access_tree proc_data loc
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
      match AccessPath.of_exp rhs_exp rhs_typ ~f_resolve_id with
      | Some rhs_access_path ->
          let id_map' = IdMapDomain.add lhs_id rhs_access_path id_map in
          { astate with Domain.id_map = id_map'; }
      | None ->
          astate

    let add_source source ret_id ret_typ access_tree =
      let trace = TraceDomain.of_source source in
      let id_ap = AccessPath.Exact (AccessPath.of_id ret_id ret_typ) in
      TaintDomain.add_trace id_ap trace access_tree

    let add_sinks sinks actuals ({ Domain.access_tree; id_map; } as astate) proc_data loc =
      let f_resolve_id = resolve_id id_map in
      (* add [sink] to the trace associated with the [formal_num]th actual *)
      let add_sink_to_actual access_tree_acc (sink_param : TraceDomain.Sink.t Sink.parameter) =
        let actual_exp, actual_typ = IList.nth actuals sink_param.index in
        match AccessPath.of_exp actual_exp actual_typ ~f_resolve_id with
        | Some actual_ap ->
            let actual_ap =
              if sink_param.report_reachable
              then AccessPath.Abstracted actual_ap
              else AccessPath.Exact actual_ap in
            begin
              match access_path_get_node actual_ap access_tree_acc proc_data loc with
              | Some (actual_trace, _) ->
                  (* add callee_pname to actual trace as a sink *)
                  let actual_trace' = TraceDomain.add_sink sink_param.sink actual_trace in
                  let pname = Cfg.Procdesc.get_proc_name proc_data.ProcData.pdesc in
                  IList.iter
                    (Reporting.log_error pname ~loc)
                    (TraceDomain.get_reportable_exns actual_trace');
                  TaintDomain.add_trace actual_ap actual_trace' access_tree_acc
              | None ->
                  access_tree_acc
            end
        | None ->
            access_tree_acc in
      let access_tree' = IList.fold_left add_sink_to_actual access_tree sinks in
      { astate with Domain.access_tree = access_tree'; }

    let exec_instr ({ Domain.id_map; } as astate) proc_data _ instr =
      let f_resolve_id = resolve_id id_map in
      match instr with
      | Sil.Load (lhs_id, rhs_exp, rhs_typ, _) ->
          analyze_id_assignment (Var.of_id lhs_id) rhs_exp rhs_typ astate
      | Sil.Store (Exp.Lvar lhs_pvar, lhs_typ, rhs_exp, _) when Pvar.is_frontend_tmp lhs_pvar ->
          analyze_id_assignment (Var.of_pvar lhs_pvar) rhs_exp lhs_typ astate
      | Sil.Store (lhs_exp, lhs_typ, rhs_exp, loc) ->
          let lhs_access_path =
            match AccessPath.of_exp lhs_exp lhs_typ ~f_resolve_id with
            | Some access_path ->
                access_path
            | None ->
                failwithf
                  "Assignment to unexpected lhs expression %a in proc %a at loc %a"
                  (Sil.pp_exp pe_text) lhs_exp
                  Procname.pp (Cfg.Procdesc.get_proc_name (proc_data.ProcData.pdesc))
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
      | Sil.Call ([ret_id], Const (Cfun callee_pname), args, loc, _)
        when Builtin.is_registered callee_pname ->
          if Procname.equal callee_pname ModelBuiltins.__cast
          then
            match args with
            | (cast_target, cast_typ) :: _ ->
                analyze_id_assignment (Var.of_id ret_id) cast_target cast_typ astate
            | _ ->
                failwithf
                  "Unexpected cast %a in procedure %a at line %a"
                  (Sil.pp_instr pe_text) instr
                  Procname.pp (Cfg.Procdesc.get_proc_name (proc_data.ProcData.pdesc))
                  Location.pp loc
          else
            astate
      | Sil.Call (ret_ids, Const (Cfun callee_pname), actuals, callee_loc, _) ->
          let call_site = CallSite.make callee_pname callee_loc in

          let astate_with_sink =
            match TraceDomain.Sink.get call_site with
            | [] -> astate
            | sinks -> add_sinks sinks actuals astate proc_data callee_loc in

          let ret_typ =
            match callee_pname with
            | Procname.Java java_pname ->
                Typ.java_proc_return_typ java_pname
            | Procname.C _ ->
                Typ.Tvoid (* for tests only, since tests use C-style procnames *)
            | _ ->
                failwith "Unimp: looking up return type for non-Java procedure" in

          let astate_with_source =
            match TraceDomain.Source.get call_site, ret_ids with
            | [(0, source)], [ret_id] ->
                let access_tree = add_source source ret_id ret_typ astate_with_sink.access_tree in
                { astate_with_sink with access_tree; }
            | [], _ |  _, [] ->
                astate_with_sink
            | _ ->
                (* this is allowed by SIL, but not currently used in any frontends *)
                failwith "Unimp: handling multiple return ids" in
          astate_with_source
      | Sil.Call _ ->
          failwith "Unimp: non-pname call expressions"
      | Sil.Prune _ | Remove_temps _ | Nullify _ | Abstract _ | Declare_locals _ ->
          astate
  end

  module Analyzer = AbstractInterpreter.Make
      (ProcCfg.Normal)
      (Scheduler.ReversePostorder)
      (TransferFunctions)

  (** grab footprint traces in [access_tree] and make them into inputs for the summary. for each
      trace Footprint(T_out, a.b.c) associated with access path x.z.y, we will produce a summary of
      the form (x.z.y, T_in) => (T_in + T_out, a.b.c) *)
  let make_summary formals access_tree pdesc =
    let add_summary_for_output summary_acc output trace =
      let make_in_out_summary summary_input summary_output trace =
        let _ = match summary_input with
          | QuandarySummary.In_formal (_, _) -> ()
          | _ -> () in
        QuandarySummary.make_in_out_summary
          summary_input summary_output (TraceDomain.to_summary_trace trace) in

      let extract_input source acc =
        let get_formal_number base formal_bases =
          IList.find_mapi_opt
            (fun index formal_base ->
               if AccessPath.base_equal base formal_base
               then Some index
               else None)
            formal_bases in

        match TraceDomain.Source.get_footprint_access_path source with
        | Some footprint_ap ->
            let input =
              let footprint_ap_base = fst (AccessPath.extract footprint_ap) in
              match get_formal_number footprint_ap_base formals with
              | Some index ->
                  QuandarySummary.make_formal_input index footprint_ap
              | None ->
                  failwithf
                    "Couldn't find formal number for %a@." AccessPath.pp_base footprint_ap_base in
            let summary = make_in_out_summary input output trace in
            summary :: acc
        | None ->
            let summary = make_in_out_summary QuandarySummary.empty_input output trace in
            summary :: acc in
      TraceDomain.Source.Set.fold extract_input (TraceDomain.sources trace) summary_acc in

    let add_summaries_for_base
        ~(f_make_output : AccessPath.t -> QuandarySummary.output) base summary_acc =
      let add_summary_for_access_path summary_acc  access_path trace =
        add_summary_for_output summary_acc (f_make_output access_path) trace in

      let raw_base_ap = base, [] in
      match TaintDomain.BaseMap.find base access_tree with
      | trace, TaintDomain.Star ->
          add_summary_for_access_path summary_acc (AccessPath.Abstracted raw_base_ap) trace
      | trace, TaintDomain.Subtree subtree ->
          let access_path = AccessPath.Exact raw_base_ap in
          add_summary_for_output summary_acc (f_make_output access_path) trace
          |> TaintDomain.access_map_fold add_summary_for_access_path base subtree
      | exception Not_found ->
          summary_acc in

    let add_formal_summaries summary_acc formal_index formal =
      let f_make_output = QuandarySummary.make_formal_output formal_index in
      add_summaries_for_base ~f_make_output formal summary_acc in

    let add_return_summaries summary_acc = match Cfg.Procdesc.get_ret_type pdesc with
      | Typ.Tvoid ->
          summary_acc
      | ret_typ ->
          let return_var_base = AccessPath.base_of_pvar (Cfg.Procdesc.get_ret_var pdesc) ret_typ in
          add_summaries_for_base
            ~f_make_output:QuandarySummary.make_return_output return_var_base summary_acc in

    (* add summaries for each formal and for the return value *)
    IList.fold_lefti add_formal_summaries [] formals
    |> add_return_summaries

  let checker { Callbacks.get_proc_desc; proc_name; proc_desc; tenv; } =
    let analyze_ondemand pdesc =
      let make_formal_access_paths pdesc : AccessPath.base list=
        let pname = Cfg.Procdesc.get_proc_name pdesc in
        let attrs = Cfg.Procdesc.get_attributes pdesc in
        IList.map
          (fun (name, typ) ->
             let pvar = Pvar.mk name pname in
             AccessPath.base_of_pvar pvar typ)
          attrs.ProcAttributes.formals in
      let formals = make_formal_access_paths pdesc in
      let proc_data = ProcData.make pdesc tenv formals in
      match Analyzer.compute_post proc_data with
      | Some { access_tree; } ->
          let summary = make_summary formals access_tree pdesc in
          Summary.write_summary (Cfg.Procdesc.get_proc_name pdesc) summary;
      | None ->
          failwith "Couldn't compute post" in
    let callbacks =
      {
        Ondemand.analyze_ondemand;
        get_proc_desc;
      } in
    if Ondemand.procedure_should_be_analyzed proc_name
    then
      begin
        Ondemand.set_callbacks callbacks;
        analyze_ondemand proc_desc;
        Ondemand.unset_callbacks ();
      end

end

module Java = Make(struct
    include JavaTrace

    let to_summary_trace trace = QuandarySummary.Java trace

    let of_summary_trace = function
      | QuandarySummary.Java trace -> trace
      | QuandarySummary.Unknown -> assert false
  end)
