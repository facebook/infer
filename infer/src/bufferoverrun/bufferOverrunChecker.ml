(*
 * Copyright (c) 2016-present
 *
 * Programming Research Laboratory (ROPAS)
 * Seoul National University, Korea
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open AbsLoc
open! AbstractDomain.Types
module BoUtils = BufferOverrunUtils
module Dom = BufferOverrunDomain
module Relation = BufferOverrunDomainRelation
module L = Logging
module Models = BufferOverrunModels
module Sem = BufferOverrunSemantics
module Trace = BufferOverrunTrace
module TraceSet = Trace.Set

module Payload = SummaryPayload.Make (struct
  type t = BufferOverrunSummary.t

  let update_payloads astate (payloads : Payloads.t) = {payloads with buffer_overrun= Some astate}

  let of_payloads (payloads : Payloads.t) = payloads.buffer_overrun
end)

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Dom.Mem

  type extras = Itv.SymbolTable.t

  let instantiate_ret (id, _) callee_pname ~callee_exit_mem eval_sym_trace
      eval_locs_sympath_partial mem location =
    let copy_reachable_new_locs_from locs mem =
      let copy loc acc =
        Option.value_map (Dom.Mem.find_opt loc callee_exit_mem) ~default:acc ~f:(fun v ->
            let v =
              Dom.Val.subst v eval_sym_trace location
              |> Dom.Val.add_trace_elem (Trace.Return location)
            in
            Dom.Mem.add_heap loc v acc )
      in
      let reachable_locs = Dom.Mem.get_reachable_locs_from locs callee_exit_mem in
      PowLoc.fold copy reachable_locs mem
    in
    let instantiate_ret_alias mem =
      let subst_loc l =
        Option.find_map (Loc.get_path l) ~f:(fun partial ->
            try
              let locs = eval_locs_sympath_partial partial in
              if PowLoc.is_singleton locs then Some (PowLoc.choose locs) else None
            with Caml.Not_found -> None )
      in
      let ret_alias =
        Option.find_map (Dom.Mem.find_ret_alias callee_exit_mem) ~f:(fun alias_target ->
            Dom.AliasTarget.loc_map alias_target ~f:subst_loc )
      in
      Option.value_map ret_alias ~default:mem ~f:(fun l -> Dom.Mem.load_alias id l mem)
    in
    let ret_loc = Loc.of_pvar (Pvar.get_ret_pvar callee_pname) in
    let ret_val = Dom.Mem.find ret_loc callee_exit_mem in
    let ret_var = Loc.of_var (Var.of_id id) in
    Dom.Val.subst ret_val eval_sym_trace location
    |> Dom.Val.add_trace_elem (Trace.Return location)
    |> Fn.flip (Dom.Mem.add_stack ret_var) mem
    |> instantiate_ret_alias
    |> copy_reachable_new_locs_from (Dom.Val.get_all_locs ret_val)


  let instantiate_param tenv pdesc params callee_exit_mem eval_sym_trace location mem =
    let formals = Sem.get_formals pdesc in
    let actuals = List.map ~f:(fun (a, _) -> Sem.eval a mem) params in
    let f mem formal actual =
      match (snd formal).Typ.desc with
      | Typ.Tptr (typ, _) -> (
        match typ.Typ.desc with
        | Typ.Tstruct typename -> (
          match Tenv.lookup tenv typename with
          | Some str ->
              let formal_locs =
                Dom.Mem.find (Loc.of_pvar (fst formal)) callee_exit_mem
                |> Dom.Val.get_array_blk |> ArrayBlk.get_pow_loc
              in
              let instantiate_fld mem (fn, _, _) =
                let formal_fields = PowLoc.append_field formal_locs ~fn in
                let v = Dom.Mem.find_set formal_fields callee_exit_mem in
                let actual_fields = PowLoc.append_field (Dom.Val.get_all_locs actual) ~fn in
                Dom.Val.subst v eval_sym_trace location
                |> Fn.flip (Dom.Mem.strong_update actual_fields) mem
              in
              List.fold ~f:instantiate_fld ~init:mem str.Typ.Struct.fields
          | _ ->
              mem )
        | _ ->
            let formal_locs =
              Dom.Mem.find (Loc.of_pvar (fst formal)) callee_exit_mem
              |> Dom.Val.get_array_blk |> ArrayBlk.get_pow_loc
            in
            let v = Dom.Mem.find_set formal_locs callee_exit_mem in
            let actual_locs = Dom.Val.get_all_locs actual in
            Dom.Val.subst v eval_sym_trace location
            |> Fn.flip (Dom.Mem.strong_update actual_locs) mem )
      | _ ->
          mem
    in
    try List.fold2_exn formals actuals ~init:mem ~f with Invalid_argument _ -> mem


  let forget_ret_relation ret callee_pname mem =
    let ret_loc = Loc.of_pvar (Pvar.get_ret_pvar callee_pname) in
    let ret_var = Loc.of_var (Var.of_id (fst ret)) in
    Dom.Mem.forget_locs (PowLoc.add ret_loc (PowLoc.singleton ret_var)) mem


  let instantiate_mem :
         Tenv.t
      -> Ident.t * Typ.t
      -> Procdesc.t
      -> Typ.Procname.t
      -> (Exp.t * Typ.t) list
      -> Dom.Mem.astate
      -> BufferOverrunSummary.t
      -> Location.t
      -> Dom.Mem.astate =
   fun tenv ret callee_pdesc callee_pname params caller_mem summary location ->
    let callee_exit_mem = BufferOverrunSummary.get_output summary in
    let rel_subst_map = Sem.get_subst_map tenv callee_pdesc params caller_mem callee_exit_mem in
    let eval_sym_trace, eval_locpath = Sem.mk_eval_sym_trace callee_pdesc params caller_mem in
    let caller_mem =
      instantiate_ret ret callee_pname ~callee_exit_mem eval_sym_trace eval_locpath caller_mem
        location
      |> instantiate_param tenv callee_pdesc params callee_exit_mem eval_sym_trace location
      |> forget_ret_relation ret callee_pname
    in
    Dom.Mem.instantiate_relation rel_subst_map ~caller:caller_mem ~callee:callee_exit_mem


  let print_debug_info : Sil.instr -> Dom.Mem.astate -> Dom.Mem.astate -> unit =
   fun instr pre post ->
    L.(debug BufferOverrun Verbose) "@\n@\n================================@\n" ;
    L.(debug BufferOverrun Verbose) "@[<v 2>Pre-state : @,%a" Dom.Mem.pp pre ;
    L.(debug BufferOverrun Verbose) "@]@\n@\n%a" (Sil.pp_instr Pp.text) instr ;
    L.(debug BufferOverrun Verbose) "@\n@\n" ;
    L.(debug BufferOverrun Verbose) "@[<v 2>Post-state : @,%a" Dom.Mem.pp post ;
    L.(debug BufferOverrun Verbose) "@]@\n" ;
    L.(debug BufferOverrun Verbose) "================================@\n@."


  let exec_instr : Dom.Mem.astate -> extras ProcData.t -> CFG.Node.t -> Sil.instr -> Dom.Mem.astate
      =
   fun mem {pdesc; tenv; extras= symbol_table} node instr ->
    let output_mem =
      match instr with
      | Load (id, _, _, _) when Ident.is_none id ->
          mem
      | Load (id, exp, _, _) ->
          BoUtils.Exec.load_val id (Sem.eval exp mem) mem
      | Store (exp1, _, exp2, location) ->
          let locs = Sem.eval exp1 mem |> Dom.Val.get_all_locs in
          let v = Sem.eval exp2 mem |> Dom.Val.add_trace_elem (Trace.Assign location) in
          let mem =
            let sym_exps =
              Dom.Relation.SymExp.of_exps ~get_int_sym_f:(Sem.get_sym_f mem)
                ~get_offset_sym_f:(Sem.get_offset_sym_f mem)
                ~get_size_sym_f:(Sem.get_size_sym_f mem) exp2
            in
            Dom.Mem.store_relation locs sym_exps mem
          in
          let mem = Dom.Mem.update_mem locs v mem in
          let mem =
            if PowLoc.is_singleton locs && not v.represents_multiple_values then
              let loc_v = PowLoc.min_elt locs in
              let pname = Procdesc.get_proc_name pdesc in
              match Typ.Procname.get_method pname with
              | "__inferbo_empty" when Loc.is_return loc_v -> (
                match Sem.get_formals pdesc with
                | [(formal, _)] ->
                    let formal_v = Dom.Mem.find (Loc.of_pvar formal) mem in
                    Dom.Mem.store_empty_alias formal_v loc_v exp2 mem
                | _ ->
                    assert false )
              | _ ->
                  Dom.Mem.store_simple_alias loc_v exp2 mem
            else mem
          in
          let mem = Dom.Mem.update_latest_prune exp1 exp2 mem in
          mem
      | Prune (exp, _, _, _) ->
          Sem.Prune.prune exp mem
      | Call (((id, _) as ret), Const (Cfun callee_pname), params, location, _) -> (
          let mem = Dom.Mem.add_stack_loc (Loc.of_id id) mem in
          match Models.Call.dispatch tenv callee_pname params with
          | Some {Models.exec} ->
              let node_hash = CFG.Node.hash node in
              let model_env =
                Models.mk_model_env callee_pname node_hash location tenv symbol_table
              in
              exec model_env ~ret mem
          | None -> (
            match Ondemand.analyze_proc_name ~caller_pdesc:pdesc callee_pname with
            | Some callee_summary -> (
              match Payload.of_summary callee_summary with
              | Some payload ->
                  let callee_pdesc = Summary.get_proc_desc callee_summary in
                  instantiate_mem tenv ret callee_pdesc callee_pname params mem payload location
              | None ->
                  (* This may happen for procedures with a biabduction model. *)
                  L.(debug BufferOverrun Verbose)
                    "/!\\ Call to %a at %a has no inferbo payload@\n" Typ.Procname.pp callee_pname
                    Location.pp location ;
                  Dom.Mem.add_unknown_from id ~callee_pname ~location mem )
            | None ->
                L.(debug BufferOverrun Verbose)
                  "/!\\ Unknown call to %a at %a@\n" Typ.Procname.pp callee_pname Location.pp
                  location ;
                Dom.Mem.add_unknown_from id ~callee_pname ~location mem ) )
      | Call ((id, _), fun_exp, _, location, _) ->
          let mem = Dom.Mem.add_stack_loc (Loc.of_id id) mem in
          let () =
            L.(debug BufferOverrun Verbose)
              "/!\\ Call to non-const function %a at %a" Exp.pp fun_exp Location.pp location
          in
          Dom.Mem.add_unknown_from_funcptr id ~location mem
      | Remove_temps (temps, _) ->
          Dom.Mem.remove_temps temps mem
      | Abstract _ | Nullify _ ->
          mem
    in
    print_debug_info instr mem output_mem ;
    output_mem


  let pp_session_name node fmt = F.fprintf fmt "bufferoverrun %a" CFG.Node.pp_id (CFG.Node.id node)
end

module CFG = ProcCfg.NormalOneInstrPerNode
module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions (CFG))

type invariant_map = Analyzer.invariant_map

module Init = struct
  let declare_symbolic_val :
         Typ.Procname.t
      -> Itv.SymbolTable.t
      -> Itv.SymbolPath.partial
      -> Tenv.t
      -> node_hash:int
      -> Location.t
      -> represents_multiple_values:bool
      -> Loc.t
      -> Typ.typ
      -> inst_num:int
      -> new_sym_num:Itv.Counter.t
      -> Dom.Mem.t
      -> Dom.Mem.t =
   fun pname symbol_table path tenv ~node_hash location ~represents_multiple_values loc typ
       ~inst_num ~new_sym_num mem ->
    let max_depth = 2 in
    let new_alloc_num = Itv.Counter.make 1 in
    let rec decl_sym_val pname path tenv ~node_hash location ~represents_multiple_values ~depth
        ~may_last_field loc typ mem =
      if depth > max_depth then mem
      else
        let depth = depth + 1 in
        match typ.Typ.desc with
        | Typ.Tint ikind ->
            let unsigned = Typ.ikind_is_unsigned ikind in
            let v =
              Dom.Val.make_sym ~represents_multiple_values ~unsigned loc pname symbol_table path
                new_sym_num location
            in
            mem |> Dom.Mem.add_heap loc v |> Dom.Mem.init_param_relation loc
        | Typ.Tfloat _ ->
            let v =
              Dom.Val.make_sym ~represents_multiple_values loc pname symbol_table path new_sym_num
                location
            in
            mem |> Dom.Mem.add_heap loc v |> Dom.Mem.init_param_relation loc
        | Typ.Tptr (typ, _) when Language.curr_language_is Java -> (
          match typ with
          | {desc= Typ.Tarray {elt}} ->
              BoUtils.Exec.decl_sym_arr
                ~decl_sym_val:(decl_sym_val ~may_last_field:false)
                BoUtils.Exec.CSymArray_Array pname symbol_table path tenv ~node_hash location
                ~represents_multiple_values ~depth loc elt ~inst_num ~new_sym_num ~new_alloc_num
                mem
          | _ ->
              BoUtils.Exec.decl_sym_java_ptr
                ~decl_sym_val:(decl_sym_val ~may_last_field:false)
                pname path tenv ~node_hash location ~represents_multiple_values ~depth loc typ
                ~inst_num ~new_alloc_num mem )
        | Typ.Tptr (typ, _) ->
            BoUtils.Exec.decl_sym_arr ~decl_sym_val:(decl_sym_val ~may_last_field)
              BoUtils.Exec.CSymArray_Pointer pname symbol_table path tenv ~node_hash location
              ~represents_multiple_values ~depth loc typ ~inst_num ~new_sym_num ~new_alloc_num mem
        | Typ.Tarray {elt; length; stride} ->
            let size =
              match length with
              | Some length when may_last_field && (IntLit.iszero length || IntLit.isone length) ->
                  None (* Will be made symbolic by [decl_sym_arr] *)
              | _ ->
                  Option.map ~f:Itv.of_int_lit length
            in
            let offset = Itv.zero in
            let stride = Option.map ~f:IntLit.to_int_exn stride in
            BoUtils.Exec.decl_sym_arr
              ~decl_sym_val:(decl_sym_val ~may_last_field:false)
              BoUtils.Exec.CSymArray_Array pname symbol_table path tenv ~node_hash location
              ~represents_multiple_values ~depth loc elt ~offset ?size ?stride ~inst_num
              ~new_sym_num ~new_alloc_num mem
        | Typ.Tstruct typename -> (
          match Models.TypName.dispatch tenv typename with
          | Some {Models.declare_symbolic} ->
              let model_env = Models.mk_model_env pname node_hash location tenv symbol_table in
              declare_symbolic ~decl_sym_val:(decl_sym_val ~may_last_field) path model_env
                ~represents_multiple_values ~depth loc ~inst_num ~new_sym_num ~new_alloc_num mem
          | None ->
              let decl_fld ~may_last_field mem (fn, typ, _) =
                let loc_fld = Loc.append_field loc ~fn in
                let path = Itv.SymbolPath.field path fn in
                decl_sym_val pname path tenv ~node_hash location ~represents_multiple_values ~depth
                  loc_fld typ ~may_last_field mem
              in
              let decl_flds str =
                IList.fold_last ~f:(decl_fld ~may_last_field:false)
                  ~f_last:(decl_fld ~may_last_field) ~init:mem str.Typ.Struct.fields
              in
              let opt_struct = Tenv.lookup tenv typename in
              Option.value_map opt_struct ~default:mem ~f:decl_flds )
        | _ ->
            if Config.bo_debug >= 3 then
              L.(debug BufferOverrun Verbose)
                "/!\\ decl_fld of unhandled type: %a at %a@." (Typ.pp Pp.text) typ Location.pp
                location ;
            mem
    in
    decl_sym_val pname path tenv ~node_hash location ~represents_multiple_values ~depth:0
      ~may_last_field:true loc typ mem


  let declare_symbolic_parameters :
         Typ.Procname.t
      -> Tenv.t
      -> node_hash:int
      -> Location.t
      -> Itv.SymbolTable.t
      -> represents_multiple_values:bool
      -> inst_num:int
      -> (Pvar.t * Typ.t) list
      -> Dom.Mem.astate
      -> Dom.Mem.astate =
   fun pname tenv ~node_hash location symbol_table ~represents_multiple_values ~inst_num formals
       mem ->
    let new_sym_num = Itv.Counter.make 0 in
    let add_formal (mem, inst_num) (pvar, typ) =
      let loc = Loc.of_pvar pvar in
      let path = Itv.SymbolPath.of_pvar pvar in
      let mem =
        declare_symbolic_val pname symbol_table path tenv ~node_hash location
          ~represents_multiple_values loc typ ~inst_num ~new_sym_num mem
      in
      (mem, inst_num + 1)
    in
    List.fold ~f:add_formal ~init:(mem, inst_num) formals |> fst


  let initial_state {ProcData.pdesc; tenv; extras= symbol_table} start_node =
    let node_hash = CFG.Node.hash start_node in
    let location = CFG.Node.loc start_node in
    let pname = Procdesc.get_proc_name pdesc in
    let rec decl_local pname ~node_hash location loc typ ~inst_num ~represents_multiple_values
        ~dimension mem =
      match typ.Typ.desc with
      | Typ.Tarray {elt= typ; length; stride} ->
          let stride = Option.map ~f:IntLit.to_int_exn stride in
          BoUtils.Exec.decl_local_array ~decl_local pname ~node_hash location loc typ ~length
            ?stride ~inst_num ~represents_multiple_values ~dimension mem
      | Typ.Tstruct typname -> (
        match Models.TypName.dispatch tenv typname with
        | Some {Models.declare_local} ->
            let model_env = Models.mk_model_env pname node_hash location tenv symbol_table in
            declare_local ~decl_local model_env loc ~inst_num ~represents_multiple_values
              ~dimension mem
        | None ->
            (mem, inst_num) )
      | _ ->
          (mem, inst_num)
    in
    let try_decl_local (mem, inst_num) {ProcAttributes.name; typ} =
      let pvar = Pvar.mk name pname in
      let loc = Loc.of_pvar pvar in
      decl_local pname ~node_hash location loc typ ~inst_num ~represents_multiple_values:false
        ~dimension:1 mem
    in
    let mem = Dom.Mem.init in
    let mem, inst_num = List.fold ~f:try_decl_local ~init:(mem, 1) (Procdesc.get_locals pdesc) in
    let formals = Sem.get_formals pdesc in
    declare_symbolic_parameters pname tenv ~node_hash location symbol_table
      ~represents_multiple_values:false ~inst_num formals mem
end

module Report = struct
  module PO = BufferOverrunProofObligations

  module ExitStatement = struct
    (* check that we are the last significant instruction
     * of a procedure (no more significant instruction)
     * or of a block (goes directly to a node with multiple predecessors)
     *)
    let rec is_end_of_block_or_procedure (cfg : CFG.t) node rem_instrs =
      Instrs.for_all rem_instrs ~f:Sil.instr_is_auxiliary
      &&
      match IContainer.singleton_or_more node ~fold:(CFG.fold_succs cfg) with
      | IContainer.Empty ->
          true
      | Singleton succ ->
          (* [succ] is a join, i.e. [node] is the end of a block *)
          IContainer.mem_nth succ 1 ~fold:(CFG.fold_preds cfg)
          || is_end_of_block_or_procedure cfg succ (CFG.instrs succ)
      | More ->
          false
  end

  let check_unreachable_code summary tenv (cfg : CFG.t) (node : CFG.Node.t) instr rem_instrs =
    match instr with
    | Sil.Prune (_, _, _, (Ik_land_lor | Ik_bexp)) ->
        ()
    | Sil.Prune (cond, location, true_branch, _) ->
        let desc =
          let err_desc =
            let i = match cond with Exp.Const (Const.Cint i) -> i | _ -> IntLit.zero in
            Errdesc.explain_condition_always_true_false tenv i cond (CFG.Node.underlying_node node)
              location
          in
          F.asprintf "%a" Localise.pp_error_desc err_desc
        in
        let issue_type =
          if true_branch then IssueType.condition_always_false else IssueType.condition_always_true
        in
        Reporting.log_warning summary ~loc:location issue_type desc
    (* special case for `exit` when we're at the end of a block / procedure *)
    | Sil.Call (_, Const (Cfun pname), _, _, _)
      when String.equal (Typ.Procname.get_method pname) "exit"
           && ExitStatement.is_end_of_block_or_procedure cfg node rem_instrs ->
        ()
    | _ ->
        let location = Sil.instr_get_loc instr in
        Reporting.log_error summary ~loc:location IssueType.unreachable_code_after
          "Unreachable code after statement"


  let check_binop_array_access :
         is_plus:bool
      -> e1:Exp.t
      -> e2:Exp.t
      -> Location.t
      -> Dom.Mem.astate
      -> PO.ConditionSet.t
      -> PO.ConditionSet.t =
   fun ~is_plus ~e1 ~e2 location mem cond_set ->
    let arr = Sem.eval e1 mem in
    let idx = Sem.eval e2 mem in
    let idx_sym_exp = Relation.SymExp.of_exp ~get_sym_f:(Sem.get_sym_f mem) e2 in
    let relation = Dom.Mem.get_relation mem in
    BoUtils.Check.array_access ~arr ~idx ~idx_sym_exp ~relation ~is_plus location cond_set


  let check_binop :
         bop:Binop.t
      -> e1:Exp.t
      -> e2:Exp.t
      -> Location.t
      -> Dom.Mem.astate
      -> PO.ConditionSet.t
      -> PO.ConditionSet.t =
   fun ~bop ~e1 ~e2 location mem cond_set ->
    match bop with
    | Binop.PlusPI ->
        check_binop_array_access ~is_plus:true ~e1 ~e2 location mem cond_set
    | Binop.MinusPI ->
        check_binop_array_access ~is_plus:false ~e1 ~e2 location mem cond_set
    | _ ->
        cond_set


  let check_expr : Exp.t -> Location.t -> Dom.Mem.astate -> PO.ConditionSet.t -> PO.ConditionSet.t
      =
   fun exp location mem cond_set ->
    let rec check_sub_expr exp cond_set =
      match exp with
      | Exp.Lindex (array_exp, index_exp) ->
          cond_set |> check_sub_expr array_exp |> check_sub_expr index_exp
          |> BoUtils.Check.lindex ~array_exp ~index_exp mem location
      | Exp.BinOp (_, e1, e2) ->
          cond_set |> check_sub_expr e1 |> check_sub_expr e2
      | Exp.Lfield (e, _, _) | Exp.UnOp (_, e, _) | Exp.Exn e | Exp.Cast (_, e) ->
          check_sub_expr e cond_set
      | Exp.Closure {captured_vars} ->
          List.fold captured_vars ~init:cond_set ~f:(fun cond_set (e, _, _) ->
              check_sub_expr e cond_set )
      | Exp.Var _ | Exp.Lvar _ | Exp.Const _ | Exp.Sizeof _ ->
          cond_set
    in
    let cond_set = check_sub_expr exp cond_set in
    match exp with
    | Exp.Var _ ->
        let arr = Sem.eval exp mem in
        let idx, idx_sym_exp = (Dom.Val.Itv.zero, Some Relation.SymExp.zero) in
        let relation = Dom.Mem.get_relation mem in
        BoUtils.Check.array_access ~arr ~idx ~idx_sym_exp ~relation ~is_plus:true location cond_set
    | Exp.BinOp (bop, e1, e2) ->
        check_binop ~bop ~e1 ~e2 location mem cond_set
    | _ ->
        cond_set


  let instantiate_cond :
         Tenv.t
      -> Procdesc.t
      -> (Exp.t * Typ.t) list
      -> Dom.Mem.astate
      -> Payload.t
      -> Location.t
      -> PO.ConditionSet.t =
   fun tenv callee_pdesc params caller_mem summary location ->
    let callee_exit_mem = BufferOverrunSummary.get_output summary in
    let callee_cond = BufferOverrunSummary.get_cond_set summary in
    let rel_subst_map = Sem.get_subst_map tenv callee_pdesc params caller_mem callee_exit_mem in
    let pname = Procdesc.get_proc_name callee_pdesc in
    let caller_rel = Dom.Mem.get_relation caller_mem in
    let eval_sym_trace, _ = Sem.mk_eval_sym_trace callee_pdesc params caller_mem in
    PO.ConditionSet.subst callee_cond eval_sym_trace rel_subst_map caller_rel pname location


  let check_instr :
         Procdesc.t
      -> Tenv.t
      -> Itv.SymbolTable.t
      -> CFG.Node.t
      -> Sil.instr
      -> Dom.Mem.astate
      -> PO.ConditionSet.t
      -> PO.ConditionSet.t =
   fun pdesc tenv symbol_table node instr mem cond_set ->
    match instr with
    | Sil.Load (_, exp, _, location) | Sil.Store (exp, _, _, location) ->
        check_expr exp location mem cond_set
    | Sil.Call (_, Const (Cfun callee_pname), params, location, _) -> (
      match Models.Call.dispatch tenv callee_pname params with
      | Some {Models.check} ->
          let node_hash = CFG.Node.hash node in
          let pname = Procdesc.get_proc_name pdesc in
          check (Models.mk_model_env pname node_hash location tenv symbol_table) mem cond_set
      | None -> (
        match Ondemand.analyze_proc_name ~caller_pdesc:pdesc callee_pname with
        | Some callee_summary -> (
          match Payload.of_summary callee_summary with
          | Some callee_payload ->
              let callee_pdesc = Summary.get_proc_desc callee_summary in
              instantiate_cond tenv callee_pdesc params mem callee_payload location
              |> PO.ConditionSet.join cond_set
          | None ->
              (* no inferbo payload *) cond_set )
        | None ->
            (* unknown call *) cond_set ) )
    | _ ->
        cond_set


  let print_debug_info : Sil.instr -> Dom.Mem.astate -> PO.ConditionSet.t -> unit =
   fun instr pre cond_set ->
    L.(debug BufferOverrun Verbose) "@\n@\n================================@\n" ;
    L.(debug BufferOverrun Verbose) "@[<v 2>Pre-state : @,%a" Dom.Mem.pp pre ;
    L.(debug BufferOverrun Verbose) "@]@\n@\n%a" (Sil.pp_instr Pp.text) instr ;
    L.(debug BufferOverrun Verbose) "@[<v 2>@\n@\n%a" PO.ConditionSet.pp cond_set ;
    L.(debug BufferOverrun Verbose) "@]@\n" ;
    L.(debug BufferOverrun Verbose) "================================@\n@."


  let check_instrs :
         Summary.t
      -> Procdesc.t
      -> Tenv.t
      -> Itv.SymbolTable.t
      -> CFG.t
      -> CFG.Node.t
      -> Instrs.not_reversed_t
      -> Dom.Mem.astate AbstractInterpreter.State.t
      -> PO.ConditionSet.t
      -> PO.ConditionSet.t =
   fun summary pdesc tenv symbol_table cfg node instrs state cond_set ->
    match state with
    | _ when Instrs.is_empty instrs ->
        cond_set
    | {AbstractInterpreter.State.pre= Bottom} ->
        cond_set
    | {AbstractInterpreter.State.pre= NonBottom _ as pre; post} ->
        if Instrs.nth_exists instrs 1 then
          L.(die InternalError) "Did not expect several instructions" ;
        let instr = Instrs.nth_exn instrs 0 in
        let () =
          match post with
          | Bottom ->
              check_unreachable_code summary tenv cfg node instr Instrs.empty
          | NonBottom _ ->
              ()
        in
        let cond_set = check_instr pdesc tenv symbol_table node instr pre cond_set in
        print_debug_info instr pre cond_set ;
        cond_set


  let check_node :
         Summary.t
      -> Procdesc.t
      -> Tenv.t
      -> Itv.SymbolTable.t
      -> CFG.t
      -> Analyzer.invariant_map
      -> PO.ConditionSet.t
      -> CFG.Node.t
      -> PO.ConditionSet.t =
   fun summary pdesc tenv symbol_table cfg inv_map cond_set node ->
    match Analyzer.extract_state (CFG.Node.id node) inv_map with
    | Some state ->
        let instrs = CFG.instrs node in
        check_instrs summary pdesc tenv symbol_table cfg node instrs state cond_set
    | _ ->
        cond_set


  let check_proc :
         Summary.t
      -> Procdesc.t
      -> Tenv.t
      -> Itv.SymbolTable.t
      -> CFG.t
      -> Analyzer.invariant_map
      -> PO.ConditionSet.t =
   fun summary pdesc tenv symbol_table cfg inv_map ->
    CFG.fold_nodes cfg
      ~f:(check_node summary pdesc tenv symbol_table cfg inv_map)
      ~init:PO.ConditionSet.empty


  let make_err_trace : Trace.t -> string -> Errlog.loc_trace =
   fun trace issue_desc ->
    let f elem (trace, depth) =
      match elem with
      | Trace.Alloc location ->
          let desc = "Alloc: " ^ issue_desc in
          (Errlog.make_trace_element depth location desc [] :: trace, depth)
      | Trace.ArrAccess location ->
          let desc = "ArrayAccess: " ^ issue_desc in
          (Errlog.make_trace_element depth location desc [] :: trace, depth)
      | Trace.ArrDecl location ->
          (Errlog.make_trace_element depth location "ArrayDeclaration" [] :: trace, depth)
      | Trace.Assign location ->
          (Errlog.make_trace_element depth location "Assignment" [] :: trace, depth)
      | Trace.Call location ->
          (Errlog.make_trace_element depth location "Call" [] :: trace, depth + 1)
      | Trace.Return location ->
          (Errlog.make_trace_element (depth - 1) location "Return" [] :: trace, depth - 1)
      | Trace.SymAssign (loc, location) ->
          if Loc.contains_allocsite loc then (* ugly, don't show *)
            (trace, depth)
          else
            let desc = Format.asprintf "Parameter: %a" Loc.pp loc in
            (Errlog.make_trace_element depth location desc [] :: trace, depth)
      | Trace.UnknownFrom (pname_opt, location) ->
          let desc = Format.asprintf "Unknown value from: %a" Trace.pp_pname_opt pname_opt in
          (Errlog.make_trace_element depth location desc [] :: trace, depth)
    in
    List.fold_right ~f ~init:([], 0) trace.trace |> fst |> List.rev


  let report_errors : Summary.t -> PO.ConditionSet.t -> PO.ConditionSet.t =
   fun summary cond_set ->
    let report cond trace issue_type =
      let location = PO.ConditionTrace.get_report_location trace in
      let description = PO.description cond trace in
      let trace =
        match TraceSet.choose_shortest (PO.ConditionTrace.get_val_traces trace) with
        | trace ->
            make_err_trace trace description
        | exception _ ->
            [Errlog.make_trace_element 0 location description []]
      in
      Reporting.log_error summary ~loc:location ~ltr:trace issue_type description
    in
    PO.ConditionSet.check_all ~report cond_set


  let forget_locs = PO.ConditionSet.forget_locs

  let for_summary = PO.ConditionSet.for_summary
end

let extract_pre = Analyzer.extract_pre

let extract_post = Analyzer.extract_post

let print_summary : Typ.Procname.t -> BufferOverrunSummary.t -> unit =
 fun proc_name s ->
  L.(debug BufferOverrun Medium)
    "@\n@[<v 2>Summary of %a:@,%a@]@." Typ.Procname.pp proc_name BufferOverrunSummary.pp s


let get_local_decls proc_desc =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let accum_local_decls acc {ProcAttributes.name} =
    let pvar = Pvar.mk name proc_name in
    let loc = Loc.of_pvar pvar in
    PowLoc.add loc acc
  in
  Procdesc.get_locals proc_desc |> List.fold ~init:PowLoc.empty ~f:accum_local_decls


let compute_invariant_map_and_check : Callbacks.proc_callback_args -> invariant_map * Summary.t =
 fun {proc_desc; tenv; summary} ->
  Preanal.do_preanalysis proc_desc tenv ;
  let symbol_table = Itv.SymbolTable.empty () in
  let pdata = ProcData.make proc_desc tenv symbol_table in
  let cfg = CFG.from_pdesc proc_desc in
  let initial = Init.initial_state pdata (CFG.start_node cfg) in
  let inv_map = Analyzer.exec_pdesc ~initial pdata in
  let locals = get_local_decls proc_desc in
  let exit_mem =
    extract_post (CFG.exit_node cfg |> CFG.Node.id) inv_map
    |> Option.map ~f:(Dom.Mem.forget_locs locals)
  in
  let cond_set =
    Report.check_proc summary proc_desc tenv symbol_table cfg inv_map
    |> Report.report_errors summary |> Report.forget_locs locals |> Report.for_summary
  in
  let summary =
    match exit_mem with
    | Some exit_mem ->
        let post = (exit_mem, cond_set) in
        ( if Config.bo_debug >= 1 then
          let proc_name = Procdesc.get_proc_name proc_desc in
          print_summary proc_name post ) ;
        Payload.update_summary post summary
    | _ ->
        summary
  in
  (inv_map, summary)


let checker : Callbacks.proc_callback_args -> Summary.t =
 fun args -> compute_invariant_map_and_check args |> snd
