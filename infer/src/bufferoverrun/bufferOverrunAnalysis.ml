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

type analysis_data =
  { interproc: BufferOverrunAnalysisSummary.t InterproceduralAnalysis.t
  ; get_summary: BufferOverrunAnalysisSummary.get_summary
  ; get_formals: BoUtils.get_formals
  ; oenv: OndemandEnv.t }

module CFG = ProcCfg.NormalOneInstrPerNode

module Init = struct
  let initial_state {interproc= {proc_desc; tenv}; get_summary; oenv} start_node =
    let try_decl_local =
      let pname = Procdesc.get_proc_name proc_desc in
      let model_env =
        let node_hash = CFG.Node.hash start_node in
        let location = CFG.Node.loc start_node in
        let integer_type_widths = oenv.OndemandEnv.integer_type_widths in
        BoUtils.ModelEnv.mk_model_env pname ~node_hash location tenv integer_type_widths get_summary
      in
      fun (mem, inst_num) {ProcAttributes.name; typ} ->
        let loc = Loc.of_pvar (Pvar.mk name pname) in
        BoUtils.Exec.decl_local model_env (mem, inst_num) (loc, typ)
    in
    let mem = Dom.Mem.init get_summary oenv in
    let mem, _ = List.fold ~f:try_decl_local ~init:(mem, 1) (Procdesc.get_locals proc_desc) in
    mem
end

module TransferFunctions = struct
  module CFG = CFG
  module Domain = Dom.Mem

  type nonrec analysis_data = analysis_data

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
        Dom.Mem.unreachable
    | Error `SubstFail ->
        mem


  let instantiate_mem_reachable ret_id callee_formals callee_pname ~callee_exit_mem
      ({Dom.eval_locpath} as eval_sym_trace) mem location =
    let formal_locs =
      List.fold callee_formals ~init:LocSet.empty ~f:(fun acc (formal, _) ->
          LocSet.add (Loc.of_pvar formal) acc )
    in
    let copy_reachable_locs_from locs mem =
      let copy loc acc =
        Option.value_map (Dom.Mem.find_opt loc callee_exit_mem) ~default:acc ~f:(fun v ->
            let locs = PowLoc.subst_loc loc eval_locpath in
            let v = Dom.Val.subst v eval_sym_trace location in
            (* Always do strong updates if the following two conditions hold
               1) Context-sensitive allocsites are assumed: a single allocsite in a caller
                  doesn't represent objects from different call contexts. For example:

                 Arr1 = createArray (length1); – creates an array of length length1
                 Arr2 = createArray (length2); – creates an array of length length2

                 If it can happen that the allocsite representing an array created in
                 createArray is imported as the same allocsite for both calls (Arr1 and
                 Arr2 are then represented by the same allocsite), weak update has to be
                 always performed.

                 Currently, BO doesn't ensure this assumption, but frontend can ensure it
                 by copying an allocsite imported from the callee to the new one after the
                 call.

                 BO could ensure this assumption by adding context sensitivity to allocsites
                 (an identification of a caller and a call node) when importing then from a
                 callee to a caller.

               2) (2.A) Either it holds can_strong_update for imported locations or
                 (2.B) the set of imported locations consists of a single known location
                 and for locations representing multiple values it holds either that all the
                 values they represent were updated or the location has unknown value. This holds
                 when default value for a location is unknown and such location is always weekly
                 updated. *)
            Dom.Mem.update_mem
              ~force_strong_update:
                ( if Config.bo_context_sensitive_allocsites then
                    if not Config.bo_bottom_as_default then PowLoc.is_single_known_loc locs
                    else can_strong_update locs
                  else false )
              locs v acc )
      in
      let reachable_locs = Dom.Mem.get_reachable_locs_from callee_formals locs callee_exit_mem in
      LocSet.fold copy (LocSet.diff reachable_locs formal_locs) mem
    in
    let instantiate_ret_alias mem =
      let subst_loc l =
        (* TODO: for locations corresponding to parameters passed by value (e.g., IN parameters
           in Ada), subst_loc returns None meaning that ret_alias is not instantiated for them.
           The reason is that we don't have a way to import such locations from a callee to
           a caller.
        *)
        if Loc.is_global l then Some l
        else
          Option.find_map (Loc.get_param_path l) ~f:(fun partial ->
              try
                let locs = eval_locpath partial in
                match PowLoc.is_singleton_or_more locs with
                (* it is only useful to record alias to a known location, return None for unknown *)
                | IContainer.Singleton loc ->
                    Some loc
                | _ ->
                    None
              with Not_found_s _ | Caml.Not_found -> None )
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
      match Attributes.load callee_pname with
      | Some callee_attrs when callee_attrs.ProcAttributes.has_added_return_param ->
          Dom.Val.of_loc (Loc.of_pvar (Pvar.get_ret_param_pvar callee_pname))
      | _ ->
          Dom.Mem.find (Loc.of_pvar (Pvar.get_ret_pvar callee_pname)) callee_exit_mem
    in
    Dom.Mem.add_stack ret_var (Dom.Val.subst ret_val eval_sym_trace location) mem
    |> instantiate_ret_alias
    |> copy_reachable_locs_from
         (LocSet.union formal_locs (Dom.Val.get_all_locs ret_val |> PowLoc.to_set))
    |> instantiate_latest_prune ~ret_id ~callee_exit_mem eval_sym_trace location


  let instantiate_mem :
         is_args_ref:bool
      -> IntegerWidths.t
      -> Ident.t
      -> (Pvar.t * Typ.t) list
      -> Procname.t
      -> (Exp.t * Typ.t) list
      -> (Exp.t * Pvar.t * Typ.t * CapturedVar.capture_mode) list
      -> Dom.Mem.t
      -> BufferOverrunAnalysisSummary.t
      -> Location.t
      -> Dom.Mem.t =
   fun ~is_args_ref integer_type_widths ret_id callee_formals callee_pname args captured_vars
       caller_mem callee_exit_mem location ->
    let eval_sym_trace =
      Sem.mk_eval_sym_trace ~is_args_ref integer_type_widths callee_formals args captured_vars
        caller_mem ~mode:Sem.EvalNormal
    in
    let mem =
      instantiate_mem_reachable ret_id callee_formals callee_pname ~callee_exit_mem eval_sym_trace
        caller_mem location
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


  let is_java_enum_values tenv callee_pname =
    Option.exists (Procname.get_class_type_name callee_pname) ~f:(fun callee_class_name ->
        PatternMatch.Java.is_enum tenv callee_class_name
        && String.equal (Procname.get_method callee_pname) "values" )


  let assign_java_enum_values get_summary id ~caller_pname ~callee_pname mem =
    let caller_class_name = Procname.get_class_type_name caller_pname in
    let callee_class_name = Procname.get_class_type_name callee_pname in
    let is_caller_class_initializer =
      IOption.exists2 caller_class_name callee_class_name
        ~f:(fun caller_class_name callee_class_name ->
          Procname.is_java_class_initializer caller_pname
          && Typ.equal_name caller_class_name callee_class_name )
    in
    match callee_class_name with
    | Some (JavaClass class_name as typename) ->
        let clinit_mem =
          if is_caller_class_initializer then Some (Dom.Mem.unset_oenv mem)
          else get_summary (Procname.Java (Procname.Java.get_class_initializer typename))
        in
        Option.value_map clinit_mem ~default:mem ~f:(fun clinit_mem ->
            let loc =
              let class_var =
                let class_mangled = Mangled.from_string (JavaClassName.to_string class_name) in
                Loc.of_var (Var.of_pvar (Pvar.mk_global class_mangled))
              in
              let fn = Fieldname.make typename "$VALUES" in
              Loc.append_field class_var fn
            in
            let v = Dom.Mem.find loc clinit_mem in
            let mem = Dom.Mem.add_stack (Loc.of_id id) v mem in
            if is_caller_class_initializer then mem
            else
              let arr_locs = Dom.Val.get_all_locs v in
              let arr_v = Dom.Mem.find_set arr_locs clinit_mem in
              Dom.Mem.add_heap_set arr_locs arr_v mem )
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
      let reachable_locs = Dom.Mem.get_reachable_locs_from [] (LocSet.singleton loc) from_mem in
      LocSet.fold copy reachable_locs to_mem
    in
    fun tenv get_summary exp mem ->
      Option.value_map (Exp.get_java_class_initializer tenv exp) ~default:mem
        ~f:(fun (clinit_pname, pvar, fn, field_typ) ->
          let copy_from_class_init () =
            Option.value_map (get_summary clinit_pname) ~default:mem ~f:(fun clinit_mem ->
                let field_loc = Loc.append_field ~typ:field_typ (Loc.of_pvar pvar) fn in
                copy_reachable_locs_from field_loc ~from_mem:clinit_mem ~to_mem:mem )
          in
          match field_typ.Typ.desc with
          | Typ.Tptr ({desc= Tstruct _}, _) when is_known_java_static_field fn ->
              (* It copies all of the reachable values when the contents of the field are commonly
                 used as immutable, e.g., values of enum. *)
              copy_from_class_init ()
          | Typ.Tptr ({desc= Tarray _}, _) ->
              copy_from_class_init ()
          | _ ->
              mem )


  let java_store_linked_list_next locs v mem =
    PowLoc.get_linked_list_next ~lhs:locs ~rhs:(Dom.Val.get_all_locs v)
    |> Option.value_map ~default:mem ~f:(fun loc ->
           let linked_list_index = Loc.append_field loc BufferOverrunField.java_linked_list_index in
           let v = Dom.Mem.find linked_list_index mem |> Dom.Val.plus_a Dom.Val.Itv.one in
           Dom.Mem.add_heap linked_list_index v mem )


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


  let call
      {interproc= {proc_desc= pdesc; tenv}; get_summary; get_formals; oenv= {integer_type_widths}}
      node location ((id, _) as ret) callee_pname args captured_vars mem =
    let mem = Dom.Mem.add_stack_loc (Loc.of_id id) mem in
    let fun_arg_list =
      List.map args ~f:(fun (exp, typ) ->
          ProcnameDispatcher.Call.FuncArg.{exp; typ; arg_payload= ()} )
    in
    match Models.Call.dispatch tenv callee_pname fun_arg_list with
    | Some {Models.exec} ->
        let model_env =
          let node_hash = CFG.Node.hash node in
          BoUtils.ModelEnv.mk_model_env callee_pname ~caller_pname:(Procdesc.get_proc_name pdesc)
            ~node_hash location tenv integer_type_widths get_summary
        in
        exec model_env ~ret mem
    | None -> (
        let {BoUtils.ReplaceCallee.pname= callee_pname; args; is_args_ref} =
          BoUtils.ReplaceCallee.replace_make_shared tenv get_formals callee_pname args
        in
        match (get_summary callee_pname, get_formals callee_pname) with
        | Some callee_exit_mem, Some callee_formals ->
            instantiate_mem ~is_args_ref integer_type_widths id callee_formals callee_pname args
              captured_vars mem callee_exit_mem location
        | _, _ ->
            (* This may happen for procedures with a biabduction model too. *)
            L.d_printfln_escaped "/!\\ Unknown call to %a" Procname.pp_without_templates
              callee_pname ;
            if Config.cost_log_unknown_calls then
              ScubaLogging.log_message ~label:"unmodeled_function_inferbo"
                ~message:
                  (F.asprintf "[Inferbo] Unmodeled Function: %a" Procname.pp_without_templates
                     callee_pname ) ;
            Dom.Mem.add_unknown_from ret ~callee_pname ~location mem )


  let unknown_call location ((id, _) as ret) mem =
    let mem = Dom.Mem.add_stack_loc (Loc.of_id id) mem in
    Dom.Mem.add_unknown ret ~location mem


  let exec_instr :
         Dom.Mem.t
      -> analysis_data
      -> CFG.Node.t
      -> ProcCfg.InstrNode.instr_index
      -> Sil.instr
      -> Dom.Mem.t =
   fun mem
       ({interproc= {proc_desc; tenv}; get_summary; oenv= {integer_type_widths}} as analysis_data)
       node _ instr ->
    if not (Dom.Mem.is_reachable mem) then mem
    else
      match instr with
      | Load {id} when Ident.is_none id ->
          mem
      | Load {id; e= Exp.Lvar pvar; typ; loc= location}
        when Pvar.is_compile_constant pvar || Pvar.is_ice pvar ->
          load_global_constant get_summary (id, typ) pvar location mem
            ~find_from_initializer:(fun callee_mem -> Dom.Mem.find (Loc.of_pvar pvar) callee_mem )
      | Load {id; e= Exp.Lindex (Exp.Lvar pvar, _); typ; loc= location}
        when Pvar.is_compile_constant pvar || Pvar.is_ice pvar
             || (Pvar.is_constant_array pvar && Pvar.is_const pvar) ->
          load_global_constant get_summary (id, typ) pvar location mem
            ~find_from_initializer:(fun callee_mem ->
              let locs = Dom.Mem.find (Loc.of_pvar pvar) callee_mem |> Dom.Val.get_all_locs in
              Dom.Mem.find_set locs callee_mem )
      | Load {id; e= exp; typ; loc= location} -> (
          let model_env =
            let pname = Procdesc.get_proc_name proc_desc in
            let node_hash = CFG.Node.hash node in
            BoUtils.ModelEnv.mk_model_env pname ~node_hash location tenv integer_type_widths
              get_summary
          in
          match modeled_load_of_empty_collection_opt exp model_env (id, typ) mem with
          | Some mem' ->
              mem'
          | None ->
              let mem =
                if Language.curr_language_is Java then
                  join_java_static_final tenv get_summary exp mem
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
          let pname = Procdesc.get_proc_name proc_desc in
          let node_hash = CFG.Node.hash node in
          let model_env =
            BoUtils.ModelEnv.mk_model_env pname ~node_hash location tenv integer_type_widths
              get_summary
          in
          let tgt_locs = Sem.eval_locs tgt_exp mem in
          let tgt_deref =
            let allocsite =
              Allocsite.make pname ~caller_pname:None ~node_hash ~inst_num:1 ~dimension:1 ~path:None
                ~represents_multiple_values:false
            in
            PowLoc.singleton (Loc.of_allocsite allocsite)
          in
          Dom.Mem.update_mem tgt_locs (Dom.Val.of_pow_loc ~traces:Trace.Set.bottom tgt_deref) mem
          |> Models.JavaString.constructor_from_char_ptr model_env tgt_deref src
      | Store {e1= exp1; e2= Const (Const.Cstr s); loc= location} ->
          let locs = Sem.eval_locs exp1 mem in
          let model_env =
            let pname = Procdesc.get_proc_name proc_desc in
            let node_hash = CFG.Node.hash node in
            BoUtils.ModelEnv.mk_model_env pname ~node_hash location tenv integer_type_widths
              get_summary
          in
          let do_alloc = not (Sem.is_stack_exp exp1 mem) in
          BoUtils.Exec.decl_string model_env ~do_alloc locs s mem
      | Store {e1= exp1; typ; e2= exp2; loc= location} ->
          let locs = Sem.eval_locs exp1 mem in
          let v =
            Sem.eval integer_type_widths exp2 mem |> Dom.Val.add_assign_trace_elem location locs
          in
          let mem = Dom.Mem.update_mem locs v mem in
          let mem = java_store_linked_list_next locs v mem in
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
      | Prune (exp, location, _, _) ->
          Sem.Prune.prune location integer_type_widths exp mem
      | Call ((id, _), (Const (Cfun callee_pname) | Closure {name= callee_pname}), _, _, _)
        when is_java_enum_values tenv callee_pname ->
          let mem = Dom.Mem.add_stack_loc (Loc.of_id id) mem in
          assign_java_enum_values get_summary id
            ~caller_pname:(Procdesc.get_proc_name proc_desc)
            ~callee_pname mem
      | Call (ret, Const (Cfun callee_pname), args, location, _) ->
          call analysis_data node location ret callee_pname args [] mem
      | Call (ret, Closure {name= callee_pname; captured_vars}, args, location, _) ->
          call analysis_data node location ret callee_pname args captured_vars mem
      | Call (ret, fun_exp, args, location, _) -> (
          let func_ptrs = Sem.eval integer_type_widths fun_exp mem |> Dom.Val.get_func_ptrs in
          match FuncPtr.Set.is_singleton_or_more func_ptrs with
          | Singleton (Closure {name= callee_pname; captured_vars}) ->
              call analysis_data node location ret callee_pname args captured_vars mem
          | More ->
              L.d_printfln_escaped "/!\\ Call to multiple functions %a" Exp.pp fun_exp ;
              unknown_call location ret mem
          | Empty | Singleton (Path _) ->
              L.d_printfln_escaped "/!\\ Call to non-const function %a" Exp.pp fun_exp ;
              unknown_call location ret mem )
      | Metadata (VariableLifetimeBegins {pvar; typ; loc}) when Pvar.is_global pvar ->
          let model_env =
            let pname = Procdesc.get_proc_name proc_desc in
            let node_hash = CFG.Node.hash node in
            BoUtils.ModelEnv.mk_model_env pname ~node_hash loc tenv integer_type_widths get_summary
          in
          let mem, _ = BoUtils.Exec.decl_local model_env (mem, 1) (Loc.of_pvar pvar, typ) in
          mem
      | Metadata (ExitScope (dead_vars, _)) ->
          Dom.Mem.remove_vars dead_vars mem
      | Metadata
          ( Abstract _
          | CatchEntry _
          | EndBranches
          | Nullify _
          | Skip
          | TryEntry _
          | TryExit _
          | VariableLifetimeBegins _ ) ->
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
    BufferOverrunAnalysisSummary.t InterproceduralAnalysis.t -> invariant_map =
 fun ({InterproceduralAnalysis.proc_desc; tenv; exe_env; analyze_dependency} as interproc) ->
  let cfg = CFG.from_pdesc proc_desc in
  let analysis_data =
    let proc_name = Procdesc.get_proc_name proc_desc in
    let open IOption.Let_syntax in
    let get_summary proc_name = analyze_dependency proc_name |> AnalysisResult.to_option in
    let get_formals callee_pname =
      Attributes.load callee_pname >>| ProcAttributes.get_pvar_formals
    in
    let integer_type_widths = Exe_env.get_integer_type_widths exe_env proc_name in
    let oenv = OndemandEnv.mk proc_desc tenv integer_type_widths in
    {interproc; get_summary; get_formals; oenv}
  in
  let initial = Init.initial_state analysis_data (CFG.start_node cfg) in
  Analyzer.exec_pdesc ~do_narrowing:true ~initial analysis_data proc_desc


type cached_invariant_map = Analyzed of invariant_map | Skipped

let cached_compute_invariant_map =
  let cache_get, cache_set = Procname.UnitCache.create () in
  fun ({InterproceduralAnalysis.proc_desc} as analysis_data) ->
    let pname = Procdesc.get_proc_name proc_desc in
    match cache_get pname with
    | Some (Analyzed inv_map) ->
        Some inv_map
    | Some Skipped ->
        None
    | None
      when Procdesc.is_too_big BufferOverrunAnalysis ~max_cfg_size:Config.bo_max_cfg_size proc_desc
      ->
        cache_set pname Skipped ;
        None
    | None ->
        let inv_map = compute_invariant_map analysis_data in
        cache_set pname (Analyzed inv_map) ;
        Some inv_map


let compute_summary : (Pvar.t * Typ.t) list -> CFG.t -> invariant_map -> memory_summary =
 fun formals cfg inv_map ->
  let exit_node_id = CFG.exit_node cfg |> CFG.Node.id in
  match extract_post exit_node_id inv_map with
  | Some exit_mem ->
      exit_mem |> Dom.Mem.forget_unreachable_locs ~formals |> Dom.Mem.unset_oenv
  | None ->
      Unreachable


let analyze_procedure ({InterproceduralAnalysis.proc_desc} as analysis_data) =
  let open IOption.Let_syntax in
  let+ inv_map = cached_compute_invariant_map analysis_data in
  let formals = Procdesc.get_pvar_formals proc_desc in
  let cfg = CFG.from_pdesc proc_desc in
  compute_summary formals cfg inv_map
