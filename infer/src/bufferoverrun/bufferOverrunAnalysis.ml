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
module F = Format
module L = Logging
module Models = BufferOverrunModels
module OndemandEnv = BufferOverrunOndemandEnv
module Sem = BufferOverrunSemantics
module Trace = BufferOverrunTrace

module Payload = SummaryPayload.Make (struct
  type t = BufferOverrunAnalysisSummary.t

  let field = Payloads.Fields.buffer_overrun_analysis
end)

type extras =
  { get_summary: BufferOverrunAnalysisSummary.get_summary
  ; get_formals: BoUtils.get_formals
  ; oenv: OndemandEnv.t }

module CFG = ProcCfg.NormalOneInstrPerNode

module Init = struct
  let initial_state {ProcData.summary; tenv; extras= {get_summary; oenv}} start_node =
    let try_decl_local =
      let pname = Summary.get_proc_name summary in
      let model_env =
        let node_hash = CFG.Node.hash start_node in
        let location = CFG.Node.loc start_node in
        let integer_type_widths = oenv.OndemandEnv.integer_type_widths in
        BoUtils.ModelEnv.mk_model_env pname ~node_hash location tenv integer_type_widths
      in
      fun (mem, inst_num) {ProcAttributes.name; typ} ->
        let loc = Loc.of_pvar (Pvar.mk name pname) in
        BoUtils.Exec.decl_local model_env (mem, inst_num) (loc, typ)
    in
    let mem = Dom.Mem.init get_summary oenv in
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


  let symbolic_pname_value pname params typ location mem =
    let obj_path =
      match params with
      | (param, _) :: _ ->
          PowLoc.min_elt_opt (Sem.eval_locs param mem) |> Option.bind ~f:Loc.get_path
      | _ ->
          None
    in
    let path = Symb.SymbolPath.of_callsite ?obj_path ~ret_typ:typ (CallSite.make pname location) in
    Dom.Mem.find (Loc.of_allocsite (Allocsite.make_symbol path)) mem


  let instantiate_mem_reachable (ret_id, ret_typ) callee_formals callee_pname params
      ~callee_exit_mem ({Dom.eval_locpath} as eval_sym_trace) mem location =
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
            symbolic_pname_value callee_pname params ret_typ location mem
          else Dom.Mem.find (Loc.of_pvar (Pvar.get_ret_pvar callee_pname)) callee_exit_mem
    in
    Dom.Mem.add_stack ret_var (Dom.Val.subst ret_val eval_sym_trace location) mem
    |> instantiate_ret_alias
    |> copy_reachable_locs_from (PowLoc.join formal_locs (Dom.Val.get_all_locs ret_val))
    |> instantiate_latest_prune ~ret_id ~callee_exit_mem eval_sym_trace location


  let instantiate_mem :
         is_params_ref:bool
      -> Typ.IntegerWidths.t
      -> Ident.t * Typ.t
      -> (Pvar.t * Typ.t) list
      -> Procname.t
      -> (Exp.t * Typ.t) list
      -> Dom.Mem.t
      -> BufferOverrunAnalysisSummary.t
      -> Location.t
      -> Dom.Mem.t =
   fun ~is_params_ref integer_type_widths ret callee_formals callee_pname params caller_mem
       callee_exit_mem location ->
    let eval_sym_trace =
      Sem.mk_eval_sym_trace ~is_params_ref integer_type_widths callee_formals params caller_mem
        ~mode:Sem.EvalNormal
    in
    let mem =
      instantiate_mem_reachable ret callee_formals callee_pname params ~callee_exit_mem
        eval_sym_trace caller_mem location
    in
    if Language.curr_language_is Java then
      Dom.Mem.incr_iterator_simple_alias_on_call eval_sym_trace ~callee_exit_mem mem
    else mem


  let rec is_array_access_exp = function
    | Exp.BinOp ((PlusPI | MinusPI), _, _) | Exp.Lindex _ ->
        true
    | Exp.Cast (_, x) ->
        is_array_access_exp x
    | _ ->
        false


  let is_java_enum_values ~caller_pname ~callee_pname =
    match
      (Procname.get_class_type_name caller_pname, Procname.get_class_type_name callee_pname)
    with
    | Some caller_class_name, Some callee_class_name
      when Typ.equal_name caller_class_name callee_class_name ->
        Procname.is_java_class_initializer caller_pname
        && String.equal (Procname.get_method callee_pname) "values"
    | _, _ ->
        false


  let assign_java_enum_values id callee_pname mem =
    match Procname.get_class_type_name callee_pname with
    | Some (JavaClass class_name as typename) ->
        let class_var =
          Loc.of_var
            (Var.of_pvar
               (Pvar.mk_global (Mangled.from_string (JavaClassName.to_string class_name))))
        in
        let fn = Fieldname.make typename "$VALUES" in
        let v = Dom.Mem.find (Loc.append_field class_var ~fn) mem in
        Dom.Mem.add_stack (Loc.of_id id) v mem
    | _ ->
        assert false


  let join_java_static_final =
    let known_java_static_fields = String.Set.of_list [".EMPTY"] in
    let is_known_java_static_field fn =
      let fieldname = Fieldname.to_string fn in
      String.Set.exists known_java_static_fields ~f:(fun suffix ->
          String.is_suffix fieldname ~suffix )
    in
    let copy_reachable_locs_from loc ~from_mem ~to_mem =
      let copy loc acc =
        Option.value_map (Dom.Mem.find_opt loc from_mem) ~default:acc ~f:(fun v ->
            Dom.Mem.add_heap loc v acc )
      in
      let reachable_locs = Dom.Mem.get_reachable_locs_from [] (PowLoc.singleton loc) from_mem in
      PowLoc.fold copy reachable_locs to_mem
    in
    fun tenv get_summary exp mem ->
      Option.value_map (Exp.get_java_class_initializer tenv exp) ~default:mem
        ~f:(fun (clinit_pname, pvar, fn, field_typ) ->
          match field_typ.Typ.desc with
          | Typ.Tptr ({desc= Tstruct _}, _) ->
              (* It copies all of the reachable values when the contents of the field are commonly
                 used as immutable, e.g., values of enum.  Otherwise, it copies only the size of
                 static final array. *)
              Option.value_map (get_summary clinit_pname) ~default:mem ~f:(fun clinit_mem ->
                  let field_loc = Loc.append_field ~typ:field_typ (Loc.of_pvar pvar) ~fn in
                  if is_known_java_static_field fn then
                    copy_reachable_locs_from field_loc ~from_mem:clinit_mem ~to_mem:mem
                  else mem )
          | _ ->
              mem )


  let modeled_load_of_empty_collection_opt =
    let known_empty_collections = String.Set.of_list ["EMPTY_LIST"; "EMPTY_SET"; "EMPTY_MAP"] in
    fun exp model_env ret mem ->
      match exp with
      | Exp.Lfield (_, fieldname, typ)
        when String.Set.mem known_empty_collections (Fieldname.get_field_name fieldname)
             && String.equal "java.util.Collections" (Typ.to_string typ) ->
          Models.Collection.create_collection model_env ~ret mem ~length:Itv.zero |> Option.some
      | _ ->
          None


  let modeled_range_of_exp location exp mem =
    match exp with
    | Exp.Lindex (arr_exp, _) ->
        let length =
          Sem.eval_array_locs_length (Sem.eval_locs arr_exp mem) mem |> Dom.Val.get_itv
        in
        Option.map (Itv.get_const length)
          ~f:(Dom.ModeledRange.of_big_int ~trace:(Bounds.BoundTrace.of_loop location))
    | _ ->
        None


  let load_global_constant get_summary ((id, _) as ret) pvar location mem ~find_from_initializer =
    match Pvar.get_initializer_pname pvar with
    | Some callee_pname -> (
      match get_summary callee_pname with
      | Some callee_mem ->
          let v = find_from_initializer callee_mem in
          Dom.Mem.add_stack (Loc.of_id id) v mem
      | None ->
          L.d_printfln_escaped "/!\\ Unknown initializer of global constant %a" (Pvar.pp Pp.text)
            pvar ;
          Dom.Mem.add_unknown_from ret ~callee_pname ~location mem )
    | None ->
        L.d_printfln_escaped "/!\\ Failed to get initializer name of global constant %a"
          (Pvar.pp Pp.text) pvar ;
        Dom.Mem.add_unknown ret ~location mem


  let exec_instr : Dom.Mem.t -> extras ProcData.t -> CFG.Node.t -> Sil.instr -> Dom.Mem.t =
   fun mem {summary; tenv; extras= {get_summary; get_formals; oenv= {integer_type_widths}}} node
       instr ->
    match instr with
    | Load {id} when Ident.is_none id ->
        mem
    | Load {id; e= Exp.Lvar pvar; typ; loc= location}
      when Pvar.is_compile_constant pvar || Pvar.is_ice pvar ->
        load_global_constant get_summary (id, typ) pvar location mem
          ~find_from_initializer:(fun callee_mem -> Dom.Mem.find (Loc.of_pvar pvar) callee_mem)
    | Load {id; e= Exp.Lindex (Exp.Lvar pvar, _); typ; loc= location}
      when Pvar.is_compile_constant pvar || Pvar.is_ice pvar ->
        load_global_constant get_summary (id, typ) pvar location mem
          ~find_from_initializer:(fun callee_mem ->
            let locs = Dom.Mem.find (Loc.of_pvar pvar) callee_mem |> Dom.Val.get_all_locs in
            Dom.Mem.find_set locs callee_mem )
    | Load {id; e= exp; typ; loc= location} -> (
        let model_env =
          let pname = Summary.get_proc_name summary in
          let node_hash = CFG.Node.hash node in
          BoUtils.ModelEnv.mk_model_env pname ~node_hash location tenv integer_type_widths
        in
        match modeled_load_of_empty_collection_opt exp model_env (id, typ) mem with
        | Some mem' ->
            mem'
        | None ->
            let mem =
              if Language.curr_language_is Java then join_java_static_final tenv get_summary exp mem
              else mem
            in
            let represents_multiple_values = is_array_access_exp exp in
            let modeled_range = modeled_range_of_exp location exp mem in
            BoUtils.Exec.load_locs ~represents_multiple_values ~modeled_range id typ
              (Sem.eval_locs exp mem) mem )
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
        Dom.Mem.update_mem tgt_locs (Dom.Val.of_pow_loc ~traces:Trace.Set.bottom tgt_deref) mem
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
          let fun_arg_list =
            List.map params ~f:(fun (exp, typ) ->
                ProcnameDispatcher.Call.FuncArg.{exp; typ; arg_payload= ()} )
          in
          match Models.Call.dispatch tenv callee_pname fun_arg_list with
          | Some {Models.exec} ->
              let model_env =
                let node_hash = CFG.Node.hash node in
                BoUtils.ModelEnv.mk_model_env callee_pname ~node_hash location tenv
                  integer_type_widths
              in
              exec model_env ~ret mem
          | None -> (
              let {BoUtils.ReplaceCallee.pname= callee_pname; params; is_params_ref} =
                BoUtils.ReplaceCallee.replace_make_shared tenv get_formals callee_pname params
              in
              match (get_summary callee_pname, get_formals callee_pname) with
              | Some callee_exit_mem, Some callee_formals ->
                  instantiate_mem ~is_params_ref integer_type_widths ret callee_formals callee_pname
                    params mem callee_exit_mem location
              | _, _ ->
                  (* This may happen for procedures with a biabduction model too. *)
                  L.d_printfln_escaped "/!\\ Unknown call to %a" Procname.pp callee_pname ;
                  Dom.Mem.add_unknown_from ret ~callee_pname ~location mem ) )
    | Call (((id, _) as ret), fun_exp, _, location, _) ->
        let mem = Dom.Mem.add_stack_loc (Loc.of_id id) mem in
        L.d_printfln_escaped "/!\\ Call to non-const function %a" Exp.pp fun_exp ;
        Dom.Mem.add_unknown ret ~location mem
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

type memory_summary = BufferOverrunAnalysisSummary.t

let extract_pre = Analyzer.extract_pre

let extract_post = Analyzer.extract_post

let extract_state = Analyzer.extract_state

let compute_invariant_map :
       Summary.t
    -> Tenv.t
    -> Typ.IntegerWidths.t
    -> BufferOverrunAnalysisSummary.get_summary
    -> BoUtils.get_formals
    -> invariant_map =
 fun summary tenv integer_type_widths get_summary get_formals ->
  let pdesc = Summary.get_proc_desc summary in
  let cfg = CFG.from_pdesc pdesc in
  let pdata =
    let oenv = OndemandEnv.mk pdesc tenv integer_type_widths in
    ProcData.make summary tenv {get_summary; get_formals; oenv}
  in
  let initial = Init.initial_state pdata (CFG.start_node cfg) in
  Analyzer.exec_pdesc ~do_narrowing:true ~initial pdata


let cached_compute_invariant_map =
  (* Use a weak Hashtbl to prevent memory leaks (GC unnecessarily keeps invariant maps around) *)
  let module WeakInvMapHashTbl = Caml.Weak.Make (struct
    type t = Procname.t * invariant_map option

    let equal (pname1, _) (pname2, _) = Procname.equal pname1 pname2

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
        let get_summary callee_pname = Payload.read ~caller_summary:summary ~callee_pname in
        let get_formals callee_pname =
          Ondemand.get_proc_desc callee_pname |> Option.map ~f:Procdesc.get_pvar_formals
        in
        let inv_map =
          compute_invariant_map summary tenv integer_type_widths get_summary get_formals
        in
        WeakInvMapHashTbl.add inv_map_cache (pname, Some inv_map) ;
        inv_map


let compute_summary : (Pvar.t * Typ.t) list -> CFG.t -> invariant_map -> memory_summary =
 fun formals cfg inv_map ->
  let exit_node_id = CFG.exit_node cfg |> CFG.Node.id in
  match extract_post exit_node_id inv_map with
  | Some exit_mem ->
      exit_mem |> Dom.Mem.forget_unreachable_locs ~formals |> Dom.Mem.unset_oenv
  | None ->
      Bottom


let do_analysis : Callbacks.proc_callback_args -> Summary.t =
 fun {exe_env; summary} ->
  let proc_desc = Summary.get_proc_desc summary in
  let proc_name = Summary.get_proc_name summary in
  let tenv = Exe_env.get_tenv exe_env proc_name in
  let integer_type_widths = Exe_env.get_integer_type_widths exe_env proc_name in
  let inv_map = cached_compute_invariant_map summary tenv integer_type_widths in
  let formals = Procdesc.get_pvar_formals proc_desc in
  let cfg = CFG.from_pdesc proc_desc in
  let payload = compute_summary formals cfg inv_map in
  Payload.update_summary payload summary
