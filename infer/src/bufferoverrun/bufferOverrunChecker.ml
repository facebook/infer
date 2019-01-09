(*
 * Copyright (c) 2016-present, Programming Research Laboratory (ROPAS)
 *                             Seoul National University, Korea
 * Copyright (c) 2017-present, Facebook, Inc.
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
module TypModels = BufferOverrunTypModels

module Payload = SummaryPayload.Make (struct
  type t = BufferOverrunSummary.t

  let update_payloads astate (payloads : Payloads.t) = {payloads with buffer_overrun= Some astate}

  let of_payloads (payloads : Payloads.t) = payloads.buffer_overrun
end)

type extras = Dom.OndemandEnv.t

module CFG = ProcCfg.NormalOneInstrPerNode

module Init = struct
  let initial_state {ProcData.pdesc; tenv; extras= oenv} start_node =
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
        match TypModels.dispatch tenv typname with
        | Some (CArray {element_typ; length}) ->
            BoUtils.Exec.decl_local_array ~decl_local pname ~node_hash location loc element_typ
              ~length:(Some length) ~inst_num ~represents_multiple_values ~dimension mem
        | Some JavaCollection | None ->
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
    let mem = Dom.Mem.init oenv in
    let mem, _ = List.fold ~f:try_decl_local ~init:(mem, 1) (Procdesc.get_locals pdesc) in
    mem
end

module TransferFunctions = struct
  module CFG = CFG
  module Domain = Dom.Mem

  type nonrec extras = extras

  let instantiate_mem_reachable (ret_id, _) callee_pdesc callee_pname ~callee_exit_mem
      ({Dom.eval_locpath} as eval_sym_trace) mem location =
    let formals = Procdesc.get_pvar_formals callee_pdesc in
    let copy_reachable_locs_from locs mem =
      let copy loc acc =
        Option.value_map (Dom.Mem.find_opt loc callee_exit_mem) ~default:acc ~f:(fun v ->
            let locs = PowLoc.subst_loc loc eval_locpath in
            let v = Dom.Val.subst v eval_sym_trace location in
            PowLoc.fold (fun loc acc -> Dom.Mem.add_heap loc v acc) locs acc )
      in
      let reachable_locs = Dom.Mem.get_reachable_locs_from formals locs callee_exit_mem in
      PowLoc.fold copy reachable_locs mem
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
      let ret_alias =
        Option.find_map (Dom.Mem.find_ret_alias callee_exit_mem) ~f:(fun alias_target ->
            Dom.AliasTarget.loc_map alias_target ~f:subst_loc )
      in
      Option.value_map ret_alias ~default:mem ~f:(fun l -> Dom.Mem.load_alias ret_id l mem)
    in
    let ret_var = Loc.of_var (Var.of_id ret_id) in
    let ret_val = Dom.Mem.find (Loc.of_pvar (Pvar.get_ret_pvar callee_pname)) callee_exit_mem in
    let formal_locs =
      List.fold formals ~init:PowLoc.bot ~f:(fun acc (formal, _) ->
          let v = Dom.Mem.find (Loc.of_pvar formal) callee_exit_mem in
          PowLoc.join acc (Dom.Val.get_all_locs v) )
    in
    Dom.Mem.add_stack ret_var (Dom.Val.subst ret_val eval_sym_trace location) mem
    |> instantiate_ret_alias
    |> copy_reachable_locs_from (PowLoc.join formal_locs (Dom.Val.get_all_locs ret_val))


  let forget_ret_relation ret callee_pname mem =
    let ret_loc = Loc.of_pvar (Pvar.get_ret_pvar callee_pname) in
    let ret_var = Loc.of_var (Var.of_id (fst ret)) in
    Dom.Mem.forget_locs (PowLoc.add ret_loc (PowLoc.singleton ret_var)) mem


  let is_external pname =
    match pname with
    | Typ.Procname.Java java_pname ->
        Typ.Procname.Java.is_external java_pname
    | _ ->
        false


  let instantiate_mem :
         Tenv.t
      -> Typ.IntegerWidths.t
      -> Ident.t * Typ.t
      -> Procdesc.t
      -> Typ.Procname.t
      -> (Exp.t * Typ.t) list
      -> Dom.Mem.t
      -> BufferOverrunSummary.t
      -> Location.t
      -> Dom.Mem.t =
   fun tenv integer_type_widths ret callee_pdesc callee_pname params caller_mem summary location ->
    let callee_exit_mem = BufferOverrunSummary.get_output summary in
    let rel_subst_map =
      Sem.get_subst_map tenv integer_type_widths callee_pdesc params caller_mem callee_exit_mem
    in
    let eval_sym_trace =
      Sem.mk_eval_sym_trace integer_type_widths callee_pdesc params caller_mem ~strict:false
    in
    let caller_mem =
      instantiate_mem_reachable ret callee_pdesc callee_pname ~callee_exit_mem eval_sym_trace
        caller_mem location
      |> forget_ret_relation ret callee_pname
    in
    Dom.Mem.instantiate_relation rel_subst_map ~caller:caller_mem ~callee:callee_exit_mem


  let exec_instr : Dom.Mem.t -> extras ProcData.t -> CFG.Node.t -> Sil.instr -> Dom.Mem.t =
   fun mem {pdesc; tenv; extras= {integer_type_widths}} node instr ->
    match instr with
    | Load (id, _, _, _) when Ident.is_none id ->
        mem
    | Load (id, Exp.Lvar pvar, _, location) when Pvar.is_compile_constant pvar || Pvar.is_ice pvar
      -> (
      match Pvar.get_initializer_pname pvar with
      | Some callee_pname -> (
        match Ondemand.analyze_proc_name ~caller_pdesc:pdesc callee_pname with
        | Some callee_summary -> (
          match Payload.of_summary callee_summary with
          | Some payload ->
              let callee_mem = BufferOverrunSummary.get_output payload in
              let v = Dom.Mem.find (Loc.of_pvar pvar) callee_mem in
              Dom.Mem.add_stack (Loc.of_id id) v mem
          | None ->
              L.d_printfln_escaped "/!\\ Initializer of global constant %a has no inferbo payload"
                (Pvar.pp Pp.text) pvar ;
              Dom.Mem.add_unknown_from id ~callee_pname ~location mem )
        | None ->
            L.d_printfln_escaped "/!\\ Unknown initializer of global constant %a" (Pvar.pp Pp.text)
              pvar ;
            Dom.Mem.add_unknown_from id ~callee_pname ~location mem )
      | None ->
          L.d_printfln_escaped "/!\\ Failed to get initializer name of global constant %a"
            (Pvar.pp Pp.text) pvar ;
          Dom.Mem.add_unknown id ~location mem )
    | Load (id, exp, _, _) ->
        BoUtils.Exec.load_locs id (Sem.eval_locs exp mem) mem
    | Store (exp1, _, Const (Const.Cstr s), location) ->
        let pname = Procdesc.get_proc_name pdesc in
        let node_hash = CFG.Node.hash node in
        let locs = Sem.eval_locs exp1 mem in
        BoUtils.Exec.decl_string pname ~node_hash integer_type_widths location locs s mem
    | Store (exp1, _, exp2, location) ->
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
          if not v.represents_multiple_values then
            match PowLoc.is_singleton_or_more locs with
            | IContainer.Singleton loc_v -> (
                let pname = Procdesc.get_proc_name pdesc in
                match Typ.Procname.get_method pname with
                | "__inferbo_empty" when Loc.is_return loc_v -> (
                  match Procdesc.get_pvar_formals pdesc with
                  | [(formal, _)] ->
                      let formal_v = Dom.Mem.find (Loc.of_pvar formal) mem in
                      Dom.Mem.store_empty_alias formal_v loc_v mem
                  | _ ->
                      assert false )
                | _ ->
                    Dom.Mem.store_simple_alias loc_v exp2 mem )
            | _ ->
                mem
          else mem
        in
        let mem = Dom.Mem.update_latest_prune ~updated_locs:locs exp1 exp2 mem in
        mem
    | Prune (exp, _, _, _) ->
        Sem.Prune.prune integer_type_widths exp mem
    | Call (((id, ret_typ) as ret), Const (Cfun callee_pname), params, location, _) -> (
        let mem = Dom.Mem.add_stack_loc (Loc.of_id id) mem in
        match Models.Call.dispatch tenv callee_pname params with
        | Some {Models.exec} ->
            let node_hash = CFG.Node.hash node in
            let model_env =
              Models.mk_model_env callee_pname node_hash location tenv integer_type_widths
            in
            exec model_env ~ret mem
        | None -> (
          match Ondemand.analyze_proc_name ~caller_pdesc:pdesc callee_pname with
          | Some callee_summary -> (
            match Payload.of_summary callee_summary with
            | Some payload ->
                let callee_pdesc = Summary.get_proc_desc callee_summary in
                instantiate_mem tenv integer_type_widths ret callee_pdesc callee_pname params mem
                  payload location
            | None ->
                (* This may happen for procedures with a biabduction model. *)
                L.d_printfln_escaped "/!\\ Call to %a has no inferbo payload" Typ.Procname.pp
                  callee_pname ;
                Dom.Mem.add_unknown_from id ~callee_pname ~location mem )
          | None ->
              L.d_printfln_escaped "/!\\ Unknown call to %a" Typ.Procname.pp callee_pname ;
              if is_external callee_pname then (
                L.(debug BufferOverrun Verbose)
                  "/!\\ External call to unknown  %a \n\n" Typ.Procname.pp callee_pname ;
                let callsite = CallSite.make callee_pname location in
                let path = Symb.SymbolPath.of_callsite ~ret_typ callsite in
                let loc = Loc.of_allocsite (Allocsite.make_symbol path) in
                let v = Dom.Mem.find loc mem in
                Dom.Mem.add_stack (Loc.of_id id) v mem )
              else Dom.Mem.add_unknown_from id ~callee_pname ~location mem ) )
    | Call ((id, _), fun_exp, _, location, _) ->
        let mem = Dom.Mem.add_stack_loc (Loc.of_id id) mem in
        L.d_printfln_escaped "/!\\ Call to non-const function %a" Exp.pp fun_exp ;
        Dom.Mem.add_unknown id ~location mem
    | ExitScope (dead_vars, _) ->
        Dom.Mem.remove_temps (List.filter_map dead_vars ~f:Var.get_ident) mem
    | Abstract _ | Nullify _ ->
        mem


  let pp_session_name node fmt = F.fprintf fmt "bufferoverrun %a" CFG.Node.pp_id (CFG.Node.id node)
end

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

type invariant_map = Analyzer.invariant_map

(* Use a weak Hashtbl to prevent memory leaks (GC unnecessarily
   keeping invariant maps around) *)
module WeakInvMapHashTbl = Caml.Weak.Make (struct
  type t = Typ.Procname.t * invariant_map option

  let equal (pname1, _) (pname2, _) = Typ.Procname.equal pname1 pname2

  let hash (pname, _) = Hashtbl.hash pname
end)

let inv_map_cache = WeakInvMapHashTbl.create 100

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
        let ltr = [Errlog.make_trace_element 0 location "Here" []] in
        Reporting.log_warning summary ~loc:location ~ltr issue_type desc
    (* special case for `exit` when we're at the end of a block / procedure *)
    | Sil.Call (_, Const (Cfun pname), _, _, _)
      when String.equal (Typ.Procname.get_method pname) "exit"
           && ExitStatement.is_end_of_block_or_procedure cfg node rem_instrs ->
        ()
    | _ ->
        let location = Sil.instr_get_loc instr in
        let ltr = [Errlog.make_trace_element 0 location "Here" []] in
        Reporting.log_error summary ~loc:location ~ltr IssueType.unreachable_code_after
          "Unreachable code after statement"


  let check_binop_array_access :
         Typ.IntegerWidths.t
      -> is_plus:bool
      -> e1:Exp.t
      -> e2:Exp.t
      -> Location.t
      -> Dom.Mem.t
      -> PO.ConditionSet.checked_t
      -> PO.ConditionSet.checked_t =
   fun integer_type_widths ~is_plus ~e1 ~e2 location mem cond_set ->
    let arr = Sem.eval integer_type_widths e1 mem in
    let idx = Sem.eval integer_type_widths e2 mem in
    let idx_sym_exp =
      Relation.SymExp.of_exp ~get_sym_f:(Sem.get_sym_f integer_type_widths mem) e2
    in
    let relation = Dom.Mem.get_relation mem in
    let latest_prune = Dom.Mem.get_latest_prune mem in
    BoUtils.Check.array_access ~arr ~idx ~idx_sym_exp ~relation ~is_plus ~last_included:false
      ~latest_prune location cond_set


  let check_binop :
         Typ.IntegerWidths.t
      -> bop:Binop.t
      -> e1:Exp.t
      -> e2:Exp.t
      -> Location.t
      -> Dom.Mem.t
      -> PO.ConditionSet.checked_t
      -> PO.ConditionSet.checked_t =
   fun integer_type_widths ~bop ~e1 ~e2 location mem cond_set ->
    match bop with
    | Binop.PlusPI ->
        check_binop_array_access integer_type_widths ~is_plus:true ~e1 ~e2 location mem cond_set
    | Binop.MinusPI ->
        check_binop_array_access integer_type_widths ~is_plus:false ~e1 ~e2 location mem cond_set
    | _ ->
        cond_set


  let check_expr_for_array_access :
         Typ.IntegerWidths.t
      -> Exp.t
      -> Location.t
      -> Dom.Mem.t
      -> PO.ConditionSet.checked_t
      -> PO.ConditionSet.checked_t =
   fun integer_type_widths exp location mem cond_set ->
    let rec check_sub_expr exp cond_set =
      match exp with
      | Exp.Lindex (array_exp, index_exp) ->
          cond_set |> check_sub_expr array_exp |> check_sub_expr index_exp
          |> BoUtils.Check.lindex integer_type_widths ~array_exp ~index_exp ~last_included:false
               mem location
      | Exp.BinOp (_, e1, e2) ->
          cond_set |> check_sub_expr e1 |> check_sub_expr e2
      | Exp.Lfield (e, _, _) | Exp.UnOp (_, e, _) | Exp.Exn e ->
          check_sub_expr e cond_set
      | Exp.Cast (_, e) ->
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
        let arr = Sem.eval integer_type_widths exp mem in
        let idx, idx_sym_exp = (Dom.Val.Itv.zero, Some Relation.SymExp.zero) in
        let relation = Dom.Mem.get_relation mem in
        let latest_prune = Dom.Mem.get_latest_prune mem in
        BoUtils.Check.array_access ~arr ~idx ~idx_sym_exp ~relation ~is_plus:true
          ~last_included:false ~latest_prune location cond_set
    | Exp.BinOp (bop, e1, e2) ->
        check_binop integer_type_widths ~bop ~e1 ~e2 location mem cond_set
    | _ ->
        cond_set


  let check_binop_for_integer_overflow integer_type_widths bop ~lhs ~rhs location mem cond_set =
    match bop with
    | Binop.PlusA (Some _) | Binop.MinusA (Some _) | Binop.Mult (Some _) ->
        let lhs_v = Sem.eval integer_type_widths lhs mem in
        let rhs_v = Sem.eval integer_type_widths rhs mem in
        let latest_prune = Dom.Mem.get_latest_prune mem in
        BoUtils.Check.binary_operation integer_type_widths bop ~lhs:lhs_v ~rhs:rhs_v ~latest_prune
          location cond_set
    | _ ->
        cond_set


  let rec check_expr_for_integer_overflow integer_type_widths exp location mem cond_set =
    match exp with
    | Exp.UnOp (_, e, _)
    | Exp.Exn e
    | Exp.Lfield (e, _, _)
    | Exp.Cast (_, e)
    | Exp.Sizeof {dynamic_length= Some e} ->
        check_expr_for_integer_overflow integer_type_widths e location mem cond_set
    | Exp.BinOp (bop, lhs, rhs) ->
        cond_set
        |> check_binop_for_integer_overflow integer_type_widths bop ~lhs ~rhs location mem
        |> check_expr_for_integer_overflow integer_type_widths lhs location mem
        |> check_expr_for_integer_overflow integer_type_widths rhs location mem
    | Exp.Lindex (e1, e2) ->
        cond_set
        |> check_expr_for_integer_overflow integer_type_widths e1 location mem
        |> check_expr_for_integer_overflow integer_type_widths e2 location mem
    | Exp.Closure {captured_vars} ->
        List.fold captured_vars ~init:cond_set ~f:(fun cond_set (e, _, _) ->
            check_expr_for_integer_overflow integer_type_widths e location mem cond_set )
    | Exp.Var _ | Exp.Const _ | Exp.Lvar _ | Exp.Sizeof {dynamic_length= None} ->
        cond_set


  let instantiate_cond :
         Tenv.t
      -> Typ.IntegerWidths.t
      -> Procdesc.t
      -> (Exp.t * Typ.t) list
      -> Dom.Mem.t
      -> Payload.t
      -> Location.t
      -> PO.ConditionSet.checked_t =
   fun tenv integer_type_widths callee_pdesc params caller_mem summary location ->
    let callee_exit_mem = BufferOverrunSummary.get_output summary in
    let callee_cond = BufferOverrunSummary.get_cond_set summary in
    let rel_subst_map =
      Sem.get_subst_map tenv integer_type_widths callee_pdesc params caller_mem callee_exit_mem
    in
    let pname = Procdesc.get_proc_name callee_pdesc in
    let caller_rel = Dom.Mem.get_relation caller_mem in
    let eval_sym_trace =
      Sem.mk_eval_sym_trace integer_type_widths callee_pdesc params caller_mem
    in
    PO.ConditionSet.subst callee_cond eval_sym_trace rel_subst_map caller_rel pname location


  let check_instr :
         Procdesc.t
      -> Tenv.t
      -> Typ.IntegerWidths.t
      -> CFG.Node.t
      -> Sil.instr
      -> Dom.Mem.t
      -> PO.ConditionSet.checked_t
      -> PO.ConditionSet.checked_t =
   fun pdesc tenv integer_type_widths node instr mem cond_set ->
    match instr with
    | Sil.Load (_, exp, _, location) ->
        cond_set
        |> check_expr_for_array_access integer_type_widths exp location mem
        |> check_expr_for_integer_overflow integer_type_widths exp location mem
    | Sil.Store (lexp, _, rexp, location) ->
        cond_set
        |> check_expr_for_array_access integer_type_widths lexp location mem
        |> check_expr_for_integer_overflow integer_type_widths lexp location mem
        |> check_expr_for_integer_overflow integer_type_widths rexp location mem
    | Sil.Call (_, Const (Cfun callee_pname), params, location, _) -> (
        let cond_set =
          List.fold params ~init:cond_set ~f:(fun cond_set (exp, _) ->
              check_expr_for_integer_overflow integer_type_widths exp location mem cond_set )
        in
        match Models.Call.dispatch tenv callee_pname params with
        | Some {Models.check} ->
            let node_hash = CFG.Node.hash node in
            let pname = Procdesc.get_proc_name pdesc in
            check
              (Models.mk_model_env pname node_hash location tenv integer_type_widths)
              mem cond_set
        | None -> (
          match Ondemand.analyze_proc_name ~caller_pdesc:pdesc callee_pname with
          | Some callee_summary -> (
            match Payload.of_summary callee_summary with
            | Some callee_payload ->
                let callee_pdesc = Summary.get_proc_desc callee_summary in
                instantiate_cond tenv integer_type_widths callee_pdesc params mem callee_payload
                  location
                |> PO.ConditionSet.join cond_set
            | None ->
                (* no inferbo payload *) cond_set )
          | None ->
              (* unknown call *) cond_set ) )
    | Sil.Prune (exp, location, _, _) ->
        check_expr_for_integer_overflow integer_type_widths exp location mem cond_set
    | _ ->
        cond_set


  let print_debug_info : Sil.instr -> Dom.Mem.t -> PO.ConditionSet.checked_t -> unit =
   fun instr pre cond_set ->
    L.(debug BufferOverrun Verbose) "@\n@\n================================@\n" ;
    L.(debug BufferOverrun Verbose) "@[<v 2>Pre-state : @,%a" Dom.Mem.pp pre ;
    L.(debug BufferOverrun Verbose) "@]@\n@\n%a" (Sil.pp_instr ~print_types:true Pp.text) instr ;
    L.(debug BufferOverrun Verbose) "@\n@\n@[<v 2>%a" PO.ConditionSet.pp cond_set ;
    L.(debug BufferOverrun Verbose) "@]@\n" ;
    L.(debug BufferOverrun Verbose) "================================@\n@."


  let check_instrs :
         Summary.t
      -> Procdesc.t
      -> Tenv.t
      -> Typ.IntegerWidths.t
      -> CFG.t
      -> CFG.Node.t
      -> Instrs.not_reversed_t
      -> Dom.Mem.t AbstractInterpreter.State.t
      -> PO.ConditionSet.checked_t
      -> PO.ConditionSet.checked_t =
   fun summary pdesc tenv integer_type_widths cfg node instrs state cond_set ->
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
        let cond_set = check_instr pdesc tenv integer_type_widths node instr pre cond_set in
        print_debug_info instr pre cond_set ;
        cond_set


  let check_node :
         Summary.t
      -> Procdesc.t
      -> Tenv.t
      -> Typ.IntegerWidths.t
      -> CFG.t
      -> Analyzer.invariant_map
      -> PO.ConditionSet.checked_t
      -> CFG.Node.t
      -> PO.ConditionSet.checked_t =
   fun summary pdesc tenv integer_type_widths cfg inv_map cond_set node ->
    match Analyzer.extract_state (CFG.Node.id node) inv_map with
    | Some state ->
        let instrs = CFG.instrs node in
        check_instrs summary pdesc tenv integer_type_widths cfg node instrs state cond_set
    | _ ->
        cond_set


  let check_proc :
         Summary.t
      -> Procdesc.t
      -> Tenv.t
      -> Typ.IntegerWidths.t
      -> CFG.t
      -> Analyzer.invariant_map
      -> PO.ConditionSet.checked_t =
   fun summary pdesc tenv integer_type_widths cfg inv_map ->
    CFG.fold_nodes cfg
      ~f:(check_node summary pdesc tenv integer_type_widths cfg inv_map)
      ~init:PO.ConditionSet.empty


  let report_errors : Summary.t -> PO.ConditionSet.checked_t -> PO.ConditionSet.t =
   fun summary cond_set ->
    let report cond trace issue_type =
      let location = PO.ConditionTrace.get_report_location trace in
      let description ~markup = PO.description ~markup cond trace in
      let trace =
        let description = description ~markup:false in
        Trace.Issue.make_err_trace ~description (PO.ConditionTrace.get_val_traces trace)
        |> Errlog.concat_traces
      in
      Reporting.log_error summary ~loc:location ~ltr:trace issue_type (description ~markup:true)
    in
    PO.ConditionSet.check_all ~report cond_set


  let forget_locs = PO.ConditionSet.forget_locs

  let for_summary = PO.ConditionSet.for_summary
end

let extract_pre = Analyzer.extract_pre

let extract_post = Analyzer.extract_post

let get_local_decls proc_desc =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let accum_local_decls acc {ProcAttributes.name} =
    let pvar = Pvar.mk name proc_name in
    let loc = Loc.of_pvar pvar in
    PowLoc.add loc acc
  in
  Procdesc.get_locals proc_desc |> List.fold ~init:PowLoc.empty ~f:accum_local_decls


let compute_invariant_map_and_check : Callbacks.proc_callback_args -> invariant_map * Summary.t =
 fun {proc_desc; tenv; integer_type_widths; summary} ->
  Preanal.do_preanalysis proc_desc tenv ;
  let pdata =
    let oenv = Dom.OndemandEnv.mk proc_desc tenv integer_type_widths in
    ProcData.make proc_desc tenv oenv
  in
  let cfg = CFG.from_pdesc proc_desc in
  let initial = Init.initial_state pdata (CFG.start_node cfg) in
  let inv_map = Analyzer.exec_pdesc ~do_narrowing:true ~initial pdata in
  let exit_node = CFG.exit_node cfg in
  let underlying_exit_node = CFG.Node.underlying_node exit_node in
  let pp_name f = F.pp_print_string f "bufferoverrun check" in
  NodePrinter.start_session ~pp_name underlying_exit_node ;
  let summary =
    let locals = get_local_decls proc_desc in
    let cond_set =
      Report.check_proc summary proc_desc tenv integer_type_widths cfg inv_map
      |> Report.report_errors summary |> Report.forget_locs locals |> Report.for_summary
    in
    let exit_mem = extract_post (CFG.Node.id exit_node) inv_map in
    match exit_mem with
    | Some exit_mem ->
        let exit_mem = exit_mem |> Dom.Mem.forget_locs locals |> Dom.Mem.unset_oenv in
        let payload = (exit_mem, cond_set) in
        Payload.update_summary payload summary
    | _ ->
        summary
  in
  NodePrinter.finish_session underlying_exit_node ;
  if Config.hoisting_report_only_expensive then
    let pname = Procdesc.get_proc_name proc_desc in
    WeakInvMapHashTbl.add inv_map_cache (pname, Some inv_map)
  else () ;
  (inv_map, summary)


let lookup_inv_map_cache (callback_args : Callbacks.proc_callback_args) (pname : Typ.Procname.t) :
    invariant_map =
  (* Since we are using a weak Hashtbl, represented as a set of
     (Procname) hashed values, we have to lookup with a dummy element
     *)
  match WeakInvMapHashTbl.find_opt inv_map_cache (pname, None) with
  | Some (_, Some inv_map) ->
      inv_map
  | Some (_, None) ->
      (* this should never happen *)
      assert false
  | None ->
      (* if bufferoverrun has not been run yet, run it *)
      compute_invariant_map_and_check callback_args |> fst


let checker : Callbacks.proc_callback_args -> Summary.t =
 fun args -> compute_invariant_map_and_check args |> snd
