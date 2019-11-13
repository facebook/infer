(*
 * Copyright (c) 2016-present, Programming Research Laboratory (ROPAS)
 *                             Seoul National University, Korea
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open AbsLoc
open! AbstractDomain.Types
module BoUtils = BufferOverrunUtils
module Dom = BufferOverrunDomain
module L = Logging
module Models = BufferOverrunModels
module Sem = BufferOverrunSemantics

module Payload = SummaryPayload.Make (struct
  type t = BufferOverrunAnalysisSummary.t

  let field = Payloads.Fields.buffer_overrun_analysis
end)

type summary_and_formals = BufferOverrunAnalysisSummary.t * (Pvar.t * Typ.t) list

type get_proc_summary_and_formals = Typ.Procname.t -> summary_and_formals option

type extras = {get_proc_summary_and_formals: get_proc_summary_and_formals; oenv: Dom.OndemandEnv.t}

module CFG = ProcCfg.NormalOneInstrPerNode

module Init = struct
  let initial_state {ProcData.summary; tenv; extras= {oenv}} start_node =
    let try_decl_local =
      let pname = Summary.get_proc_name summary in
      let model_env =
        let node_hash = CFG.Node.hash start_node in
        let location = CFG.Node.loc start_node in
        let integer_type_widths = oenv.Dom.OndemandEnv.integer_type_widths in
        BoUtils.ModelEnv.mk_model_env pname ~node_hash location tenv integer_type_widths
      in
      fun (mem, inst_num) {ProcAttributes.name; typ} ->
        let loc = Loc.of_pvar (Pvar.mk name pname) in
        BoUtils.Exec.decl_local model_env (mem, inst_num) (loc, typ)
    in
    let mem = Dom.Mem.init oenv in
    let mem, _ =
      List.fold ~f:try_decl_local ~init:(mem, 1)
        (Procdesc.get_locals (Summary.get_proc_desc summary))
    in
    mem
end

module TransferFunctions = struct
  module CFG = CFG
  module Domain = Dom.Mem

  type nonrec extras = extras

  let instantiate_latest_prune ~ret_id ~callee_exit_mem eval_sym_trace location mem =
    match
      Dom.Mem.get_latest_prune callee_exit_mem
      |> Dom.LatestPrune.subst ~ret_id eval_sym_trace location
    with
    | Ok latest_prune' ->
        (* Note that we are losing some precisions here, e.g., the best results should be "and" of
           caller's and callee's pruned conditions.  For now, we defer the implementation of the
           "and" since we haven't seen a case where "and" would help yet. *)
        if Dom.LatestPrune.is_top latest_prune' then mem
        else Dom.Mem.set_latest_prune latest_prune' mem
    | Error `SubstBottom ->
        Dom.Mem.bot
    | Error `SubstFail ->
        mem


  let symbolic_pname_value pname typ location mem =
    let path = CallSite.make pname location |> Symb.SymbolPath.of_callsite ~ret_typ:typ in
    Dom.Mem.find (Loc.of_allocsite (Allocsite.make_symbol path)) mem


  let assign_symbolic_pname_value pname (id, typ) location mem =
    let v = symbolic_pname_value pname typ location mem in
    Dom.Mem.add_stack (Loc.of_id id) v mem


  let instantiate_mem_reachable (ret_id, ret_typ) callee_formals callee_pname ~callee_exit_mem
      ({Dom.eval_locpath} as eval_sym_trace) mem location =
    let formal_locs =
      List.fold callee_formals ~init:PowLoc.bot ~f:(fun acc (formal, _) ->
          PowLoc.add (Loc.of_pvar formal) acc )
    in
    let copy_reachable_locs_from locs mem =
      let copy loc acc =
        Option.value_map (Dom.Mem.find_opt loc callee_exit_mem) ~default:acc ~f:(fun v ->
            let locs = PowLoc.subst_loc loc eval_locpath in
            let v = Dom.Val.subst v eval_sym_trace location in
            PowLoc.fold (fun loc acc -> Dom.Mem.add_heap loc v acc) locs acc )
      in
      let reachable_locs = Dom.Mem.get_reachable_locs_from callee_formals locs callee_exit_mem in
      PowLoc.fold copy (PowLoc.diff reachable_locs formal_locs) mem
    in
    let instantiate_ret_alias mem =
      let subst_loc l =
        Option.find_map (Loc.get_path l) ~f:(fun partial ->
            try
              let locs = eval_locpath partial in
              match PowLoc.is_singleton_or_more locs with
              | IContainer.Singleton loc ->
                  Some loc
              | _ ->
                  None
            with Caml.Not_found -> None )
      in
      match Dom.Mem.find_ret_alias callee_exit_mem with
      | Bottom ->
          mem
      | NonBottom tgts ->
          let ret_alias = Dom.AliasTargets.subst tgts ~subst_loc in
          Dom.AliasTargets.fold (Dom.Mem.load_alias ret_id) ret_alias mem
    in
    let ret_var = Loc.of_var (Var.of_id ret_id) in
    let ret_val =
      match Procdesc.load callee_pname with
      | Some callee_pdesc when Procdesc.has_added_return_param callee_pdesc ->
          Dom.Val.of_loc (Loc.of_pvar (Pvar.get_ret_param_pvar callee_pname))
      | _ ->
          if Language.curr_language_is Java && Dom.Mem.is_exc_raised callee_exit_mem then
            symbolic_pname_value callee_pname ret_typ location mem
          else Dom.Mem.find (Loc.of_pvar (Pvar.get_ret_pvar callee_pname)) callee_exit_mem
    in
    Dom.Mem.add_stack ret_var (Dom.Val.subst ret_val eval_sym_trace location) mem
    |> instantiate_ret_alias
    |> copy_reachable_locs_from (PowLoc.join formal_locs (Dom.Val.get_all_locs ret_val))
    |> instantiate_latest_prune ~ret_id ~callee_exit_mem eval_sym_trace location


  let forget_ret_relation ret callee_pname mem =
    let ret_loc = Loc.of_pvar (Pvar.get_ret_pvar callee_pname) in
    let ret_var = Loc.of_var (Var.of_id (fst ret)) in
    Dom.Mem.relation_forget_locs (PowLoc.add ret_loc (PowLoc.singleton ret_var)) mem


  let is_external pname =
    match pname with
    | Typ.Procname.Java java_pname ->
        Typ.Procname.Java.is_external java_pname
    | _ ->
        false


  let is_non_static pname =
    match pname with
    | Typ.Procname.Java java_pname ->
        not (Typ.Procname.Java.is_static java_pname)
    | _ ->
        false


  let instantiate_mem :
         Tenv.t
      -> Typ.IntegerWidths.t
      -> Ident.t * Typ.t
      -> (Pvar.t * Typ.t) list
      -> Typ.Procname.t
      -> (Exp.t * Typ.t) list
      -> Dom.Mem.t
      -> BufferOverrunAnalysisSummary.t
      -> Location.t
      -> Dom.Mem.t =
   fun tenv integer_type_widths ret callee_formals callee_pname params caller_mem callee_exit_mem
       location ->
    let eval_sym_trace =
      Sem.mk_eval_sym_trace integer_type_widths callee_formals params caller_mem
        ~mode:Sem.EvalNormal
    in
    let caller_mem' =
      instantiate_mem_reachable ret callee_formals callee_pname ~callee_exit_mem eval_sym_trace
        caller_mem location
      |> forget_ret_relation ret callee_pname
    in
    Option.value_map Config.bo_relational_domain ~default:caller_mem' ~f:(fun _ ->
        let rel_subst_map =
          Sem.get_subst_map tenv integer_type_widths callee_formals params caller_mem
            callee_exit_mem
        in
        Dom.Mem.instantiate_relation rel_subst_map ~caller:caller_mem' ~callee:callee_exit_mem )


  let rec is_array_access_exp = function
    | Exp.BinOp ((PlusPI | MinusPI), _, _) | Exp.Lindex _ ->
        true
    | Exp.Cast (_, x) ->
        is_array_access_exp x
    | _ ->
        false


  let is_java_enum_values ~caller_pname ~callee_pname =
    match
      (Typ.Procname.get_class_type_name caller_pname, Typ.Procname.get_class_type_name callee_pname)
    with
    | Some caller_class_name, Some callee_class_name
      when Typ.equal_name caller_class_name callee_class_name ->
        Typ.Procname.is_java_class_initializer caller_pname
        && String.equal (Typ.Procname.get_method callee_pname) "values"
    | _, _ ->
        false


  let assign_java_enum_values id callee_pname mem =
    match Typ.Procname.get_class_type_name callee_pname with
    | Some (JavaClass class_name) ->
        let class_var = Loc.of_var (Var.of_pvar (Pvar.mk_global class_name)) in
        let fn = Typ.Fieldname.Java.from_string (Mangled.to_string class_name ^ ".$VALUES") in
        let v = Dom.Mem.find (Loc.append_field class_var ~fn) mem in
        Dom.Mem.add_stack (Loc.of_id id) v mem
    | _ ->
        assert false


  let exec_instr : Dom.Mem.t -> extras ProcData.t -> CFG.Node.t -> Sil.instr -> Dom.Mem.t =
   fun mem {summary; tenv; extras= {get_proc_summary_and_formals; oenv= {integer_type_widths}}} node
       instr ->
    match instr with
    | Load {id} when Ident.is_none id ->
        mem
    | Load {id; e= Exp.Lvar pvar; loc= location}
      when Pvar.is_compile_constant pvar || Pvar.is_ice pvar -> (
      match Pvar.get_initializer_pname pvar with
      | Some callee_pname -> (
        match get_proc_summary_and_formals callee_pname with
        | Some (callee_mem, _) ->
            let v = Dom.Mem.find (Loc.of_pvar pvar) callee_mem in
            Dom.Mem.add_stack (Loc.of_id id) v mem
        | None ->
            L.d_printfln_escaped "/!\\ Unknown initializer of global constant %a" (Pvar.pp Pp.text)
              pvar ;
            Dom.Mem.add_unknown_from id ~callee_pname ~location mem )
      | None ->
          L.d_printfln_escaped "/!\\ Failed to get initializer name of global constant %a"
            (Pvar.pp Pp.text) pvar ;
          Dom.Mem.add_unknown id ~location mem )
    | Load {id; e= exp; typ} ->
        let represents_multiple_values = is_array_access_exp exp in
        BoUtils.Exec.load_locs ~represents_multiple_values id typ (Sem.eval_locs exp mem) mem
    | Store {e2= Exn _} when Language.curr_language_is Java ->
        Dom.Mem.exc_raised
    | Store {e1= tgt_exp; e2= Const (Const.Cstr _) as src; loc= location}
      when Language.curr_language_is Java ->
        let pname = Summary.get_proc_name summary in
        let node_hash = CFG.Node.hash node in
        let model_env =
          BoUtils.ModelEnv.mk_model_env pname ~node_hash location tenv integer_type_widths
        in
        let tgt_locs = Sem.eval_locs tgt_exp mem in
        let tgt_deref =
          let allocsite =
            Allocsite.make pname ~node_hash ~inst_num:1 ~dimension:1 ~path:None
              ~represents_multiple_values:false
          in
          PowLoc.singleton (Loc.of_allocsite allocsite)
        in
        Dom.Mem.update_mem tgt_locs (Dom.Val.of_pow_loc ~traces:Dom.TraceSet.bottom tgt_deref) mem
        |> Models.JavaString.constructor_from_char_ptr model_env tgt_deref src
    | Store {e1= exp1; e2= Const (Const.Cstr s); loc= location} ->
        let locs = Sem.eval_locs exp1 mem in
        let model_env =
          let pname = Summary.get_proc_name summary in
          let node_hash = CFG.Node.hash node in
          BoUtils.ModelEnv.mk_model_env pname ~node_hash location tenv integer_type_widths
        in
        let do_alloc = not (Sem.is_stack_exp exp1 mem) in
        BoUtils.Exec.decl_string model_env ~do_alloc locs s mem
    | Store {e1= exp1; typ; e2= exp2; loc= location} ->
        let locs = Sem.eval_locs exp1 mem in
        let v =
          Sem.eval integer_type_widths exp2 mem |> Dom.Val.add_assign_trace_elem location locs
        in
        let mem =
          let sym_exps =
            Dom.Relation.SymExp.of_exps
              ~get_int_sym_f:(Sem.get_sym_f integer_type_widths mem)
              ~get_offset_sym_f:(Sem.get_offset_sym_f integer_type_widths mem)
              ~get_size_sym_f:(Sem.get_size_sym_f integer_type_widths mem)
              exp2
          in
          Dom.Mem.store_relation locs sym_exps mem
        in
        let mem = Dom.Mem.update_mem locs v mem in
        let mem =
          if Language.curr_language_is Clang && Typ.is_char typ then
            BoUtils.Exec.set_c_strlen ~tgt:(Sem.eval integer_type_widths exp1 mem) ~src:v mem
          else mem
        in
        let mem =
          match PowLoc.is_singleton_or_more locs with
          | IContainer.Singleton loc_v ->
              Dom.Mem.store_simple_alias loc_v exp2 mem
          | _ ->
              mem
        in
        let mem = Dom.Mem.update_latest_prune ~updated_locs:locs exp1 exp2 mem in
        mem
    | Prune (exp, _, _, _) ->
        Sem.Prune.prune integer_type_widths exp mem
    | Call (((id, _) as ret), Const (Cfun callee_pname), params, location, _) -> (
        let mem = Dom.Mem.add_stack_loc (Loc.of_id id) mem in
        if is_java_enum_values ~caller_pname:(Summary.get_proc_name summary) ~callee_pname then
          assign_java_enum_values id callee_pname mem
        else
          match Models.Call.dispatch tenv callee_pname params with
          | Some {Models.exec} ->
              let model_env =
                let node_hash = CFG.Node.hash node in
                BoUtils.ModelEnv.mk_model_env callee_pname ~node_hash location tenv
                  integer_type_widths
              in
              exec model_env ~ret mem
          | None -> (
            match get_proc_summary_and_formals callee_pname with
            | Some (callee_exit_mem, callee_formals) ->
                instantiate_mem tenv integer_type_widths ret callee_formals callee_pname params mem
                  callee_exit_mem location
            | None ->
                (* This may happen for procedures with a biabduction model too. *)
                L.d_printfln_escaped "/!\\ Unknown call to %a" Typ.Procname.pp callee_pname ;
                if is_external callee_pname then (
                  L.(debug BufferOverrun Verbose)
                    "/!\\ External call to unknown  %a \n\n" Typ.Procname.pp callee_pname ;
                  assign_symbolic_pname_value callee_pname ret location mem )
                else if is_non_static callee_pname then (
                  L.(debug BufferOverrun Verbose)
                    "/!\\ Non-static call to unknown  %a \n\n" Typ.Procname.pp callee_pname ;
                  assign_symbolic_pname_value callee_pname ret location mem )
                else Dom.Mem.add_unknown_from id ~callee_pname ~location mem ) )
    | Call ((id, _), fun_exp, _, location, _) ->
        let mem = Dom.Mem.add_stack_loc (Loc.of_id id) mem in
        L.d_printfln_escaped "/!\\ Call to non-const function %a" Exp.pp fun_exp ;
        Dom.Mem.add_unknown id ~location mem
    | Metadata (VariableLifetimeBegins (pvar, typ, location)) when Pvar.is_global pvar ->
        let model_env =
          let pname = Summary.get_proc_name summary in
          let node_hash = CFG.Node.hash node in
          BoUtils.ModelEnv.mk_model_env pname ~node_hash location tenv integer_type_widths
        in
        let mem, _ = BoUtils.Exec.decl_local model_env (mem, 1) (Loc.of_pvar pvar, typ) in
        mem
    | Metadata (ExitScope (dead_vars, _)) ->
        Dom.Mem.remove_temps (List.filter_map dead_vars ~f:Var.get_ident) mem
    | Metadata (Abstract _ | Nullify _ | Skip | VariableLifetimeBegins _) ->
        mem


  let pp_session_name node fmt = F.fprintf fmt "bufferoverrun %a" CFG.Node.pp_id (CFG.Node.id node)
end

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

type invariant_map = Analyzer.invariant_map

type local_decls = PowLoc.t

type memory_summary = BufferOverrunAnalysisSummary.t

let extract_pre = Analyzer.extract_pre

let extract_post = Analyzer.extract_post

let extract_state = Analyzer.extract_state

let get_local_decls : Procdesc.t -> local_decls =
 fun proc_desc ->
  let proc_name = Procdesc.get_proc_name proc_desc in
  let accum_local_decls acc {ProcAttributes.name} =
    let pvar = Pvar.mk name proc_name in
    let loc = Loc.of_pvar pvar in
    PowLoc.add loc acc
  in
  Procdesc.get_locals proc_desc |> List.fold ~init:PowLoc.empty ~f:accum_local_decls


let compute_invariant_map :
    Summary.t -> Tenv.t -> Typ.IntegerWidths.t -> get_proc_summary_and_formals -> invariant_map =
 fun summary tenv integer_type_widths get_proc_summary_and_formals ->
  let pdesc = Summary.get_proc_desc summary in
  let cfg = CFG.from_pdesc pdesc in
  let pdata =
    let oenv = Dom.OndemandEnv.mk pdesc tenv integer_type_widths in
    ProcData.make summary tenv {get_proc_summary_and_formals; oenv}
  in
  let initial = Init.initial_state pdata (CFG.start_node cfg) in
  Analyzer.exec_pdesc ~do_narrowing:true ~initial pdata


let cached_compute_invariant_map =
  (* Use a weak Hashtbl to prevent memory leaks (GC unnecessarily keeps invariant maps around) *)
  let module WeakInvMapHashTbl = Caml.Weak.Make (struct
    type t = Typ.Procname.t * invariant_map option

    let equal (pname1, _) (pname2, _) = Typ.Procname.equal pname1 pname2

    let hash (pname, _) = Hashtbl.hash pname
  end) in
  let inv_map_cache = WeakInvMapHashTbl.create 100 in
  fun summary tenv integer_type_widths ->
    let pname = Summary.get_proc_name summary in
    match WeakInvMapHashTbl.find_opt inv_map_cache (pname, None) with
    | Some (_, Some inv_map) ->
        inv_map
    | Some (_, None) ->
        (* this should never happen *)
        assert false
    | None ->
        let get_proc_summary_and_formals callee_pname =
          Ondemand.analyze_proc_name ~caller_summary:summary callee_pname
          |> Option.bind ~f:(fun summary ->
                 Payload.of_summary summary
                 |> Option.map ~f:(fun payload ->
                        (payload, Summary.get_proc_desc summary |> Procdesc.get_pvar_formals) ) )
        in
        let inv_map =
          compute_invariant_map summary tenv integer_type_widths get_proc_summary_and_formals
        in
        WeakInvMapHashTbl.add inv_map_cache (pname, Some inv_map) ;
        inv_map


let compute_summary :
    local_decls -> (Pvar.t * Typ.t) list -> CFG.t -> invariant_map -> memory_summary =
 fun locals formals cfg inv_map ->
  let exit_node_id = CFG.exit_node cfg |> CFG.Node.id in
  match extract_post exit_node_id inv_map with
  | Some exit_mem ->
      exit_mem
      |> Dom.Mem.forget_unreachable_locs ~formals
      |> Dom.Mem.relation_forget_locs locals
      |> Dom.Mem.unset_oenv
  | None ->
      Bottom


let do_analysis : Callbacks.proc_callback_args -> Summary.t =
 fun {exe_env; summary} ->
  let proc_desc = Summary.get_proc_desc summary in
  let proc_name = Summary.get_proc_name summary in
  let tenv = Exe_env.get_tenv exe_env proc_name in
  let integer_type_widths = Exe_env.get_integer_type_widths exe_env proc_name in
  let inv_map = cached_compute_invariant_map summary tenv integer_type_widths in
  let locals = get_local_decls proc_desc in
  let formals = Procdesc.get_pvar_formals proc_desc in
  let cfg = CFG.from_pdesc proc_desc in
  let payload = compute_summary locals formals cfg inv_map in
  Payload.update_summary payload summary
