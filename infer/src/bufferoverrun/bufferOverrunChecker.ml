(*
 * Copyright (c) 2016 - present
 *
 * Programming Research Laboratory (ROPAS)
 * Seoul National University, Korea
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open AbsLoc
open! AbstractDomain.Types
module BoUtils = BufferOverrunUtils
module Dom = BufferOverrunDomain
module L = Logging
module Models = BufferOverrunModels
module Sem = BufferOverrunSemantics
module Trace = BufferOverrunTrace
module TraceSet = Trace.Set

module Summary = Summary.Make (struct
  type payload = Dom.Summary.t

  let update_payload astate (summary: Specs.summary) =
    {summary with payload= {summary.payload with buffer_overrun= Some astate}}


  let read_payload (summary: Specs.summary) = summary.payload.buffer_overrun
end)

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Dom.Mem

  type extras = ProcData.no_extras

  let declare_symbolic_val
      : Typ.Procname.t -> Tenv.t -> node_hash:int -> Location.t -> Loc.t -> Typ.typ -> inst_num:int
        -> new_sym_num:Itv.Counter.t -> Domain.t -> Domain.t =
   fun pname tenv ~node_hash location loc typ ~inst_num ~new_sym_num mem ->
    let max_depth = 2 in
    let new_alloc_num = Itv.Counter.make 1 in
    let rec decl_sym_val pname tenv ~node_hash location ~depth ~may_last_field loc typ mem =
      if depth > max_depth then mem
      else
        let depth = depth + 1 in
        match typ.Typ.desc with
        | Typ.Tint ikind ->
            let unsigned = Typ.ikind_is_unsigned ikind in
            let v =
              Dom.Val.make_sym ~unsigned pname new_sym_num
              |> Dom.Val.add_trace_elem (Trace.SymAssign (loc, location))
            in
            Dom.Mem.add_heap loc v mem
        | Typ.Tfloat _ ->
            let v =
              Dom.Val.make_sym pname new_sym_num
              |> Dom.Val.add_trace_elem (Trace.SymAssign (loc, location))
            in
            Dom.Mem.add_heap loc v mem
        | Typ.Tptr (typ, _) ->
            BoUtils.Exec.decl_sym_arr ~decl_sym_val:(decl_sym_val ~may_last_field) pname tenv
              ~node_hash location ~depth loc typ ~inst_num ~new_sym_num ~new_alloc_num mem
        | Typ.Tarray {elt; length} ->
            let size =
              match length with
              | Some length when may_last_field && (IntLit.iszero length || IntLit.isone length) ->
                  Some (Itv.make_sym pname new_sym_num)
              | _ ->
                  Option.map ~f:Itv.of_int_lit length
            in
            let offset = Itv.zero in
            BoUtils.Exec.decl_sym_arr
              ~decl_sym_val:(decl_sym_val ~may_last_field:false)
              pname tenv ~node_hash location ~depth loc elt ~offset ?size ~inst_num ~new_sym_num
              ~new_alloc_num mem
        | Typ.Tstruct typename -> (
          match Models.TypName.dispatch typename with
          | Some {Models.declare_symbolic} ->
              let model_env = Models.mk_model_env pname node_hash location tenv in
              declare_symbolic ~decl_sym_val:(decl_sym_val ~may_last_field) model_env ~depth loc
                ~inst_num ~new_sym_num ~new_alloc_num mem
          | None ->
              let decl_fld ~may_last_field mem (fn, typ, _) =
                let loc_fld = Loc.append_field loc ~fn in
                decl_sym_val pname tenv ~node_hash location ~depth loc_fld typ ~may_last_field mem
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
    decl_sym_val pname tenv ~node_hash location ~depth:0 ~may_last_field:true loc typ mem


  let declare_symbolic_parameters
      : Typ.Procname.t -> Tenv.t -> node_hash:int -> Location.t -> inst_num:int
        -> (Pvar.t * Typ.t) list -> Dom.Mem.astate -> Dom.Mem.astate =
   fun pname tenv ~node_hash location ~inst_num formals mem ->
    let new_sym_num = Itv.Counter.make 0 in
    let add_formal (mem, inst_num) (pvar, typ) =
      let loc = Loc.of_pvar pvar in
      let mem =
        declare_symbolic_val pname tenv ~node_hash location loc typ ~inst_num ~new_sym_num mem
      in
      (mem, inst_num + 1)
    in
    List.fold ~f:add_formal ~init:(mem, inst_num) formals |> fst


  let instantiate_ret ret callee_pname ~callee_entry_mem ~callee_exit_mem subst_map mem ret_alias
      location =
    let copy_reachable_new_locs_from locs mem =
      let copy loc acc =
        let v =
          Dom.Mem.find_heap loc callee_exit_mem |> (fun v -> Dom.Val.subst v subst_map location)
          |> Dom.Val.add_trace_elem (Trace.Return location)
        in
        Dom.Mem.add_heap loc v acc
      in
      let new_locs = Dom.Mem.get_new_heap_locs ~prev:callee_entry_mem ~next:callee_exit_mem in
      let reachable_locs = Dom.Mem.get_reachable_locs_from locs callee_exit_mem in
      PowLoc.fold copy (PowLoc.inter new_locs reachable_locs) mem
    in
    match ret with
    | Some (id, _) ->
        let ret_loc = Loc.of_pvar (Pvar.get_ret_pvar callee_pname) in
        let ret_val = Dom.Mem.find_heap ret_loc callee_exit_mem in
        let ret_var = Loc.of_var (Var.of_id id) in
        let add_ret_alias l = Dom.Mem.load_alias id l mem in
        let mem = Option.value_map ret_alias ~default:mem ~f:add_ret_alias in
        Dom.Val.subst ret_val subst_map location |> Dom.Val.add_trace_elem (Trace.Return location)
        |> Fn.flip (Dom.Mem.add_stack ret_var) mem
        |> copy_reachable_new_locs_from (Dom.Val.get_all_locs ret_val)
    | None ->
        mem


  let instantiate_param tenv pdesc params callee_entry_mem callee_exit_mem subst_map location mem =
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
                Dom.Mem.find_heap (Loc.of_pvar (fst formal)) callee_entry_mem
                |> Dom.Val.get_array_blk |> ArrayBlk.get_pow_loc
              in
              let instantiate_fld mem (fn, _, _) =
                let formal_fields = PowLoc.append_field formal_locs ~fn in
                let v = Dom.Mem.find_heap_set formal_fields callee_exit_mem in
                let actual_fields = PowLoc.append_field (Dom.Val.get_all_locs actual) ~fn in
                Dom.Val.subst v subst_map location
                |> Fn.flip (Dom.Mem.strong_update_heap actual_fields) mem
              in
              List.fold ~f:instantiate_fld ~init:mem str.Typ.Struct.fields
          | _ ->
              mem )
        | _ ->
            let formal_locs =
              Dom.Mem.find_heap (Loc.of_pvar (fst formal)) callee_entry_mem
              |> Dom.Val.get_array_blk |> ArrayBlk.get_pow_loc
            in
            let v = Dom.Mem.find_heap_set formal_locs callee_exit_mem in
            let actual_locs = Dom.Val.get_all_locs actual in
            Dom.Val.subst v subst_map location
            |> Fn.flip (Dom.Mem.strong_update_heap actual_locs) mem )
      | _ ->
          mem
    in
    try List.fold2_exn formals actuals ~init:mem ~f with Invalid_argument _ -> mem


  let instantiate_mem
      : Tenv.t -> (Ident.t * Typ.t) option -> Procdesc.t option -> Typ.Procname.t
        -> (Exp.t * Typ.t) list -> Dom.Mem.astate -> Dom.Summary.t -> Location.t -> Dom.Mem.astate =
   fun tenv ret callee_pdesc callee_pname params caller_mem summary location ->
    let callee_entry_mem = Dom.Summary.get_input summary in
    let callee_exit_mem = Dom.Summary.get_output summary in
    let callee_ret_alias = Dom.Mem.find_ret_alias callee_exit_mem in
    match callee_pdesc with
    | Some pdesc ->
        let subst_map, ret_alias =
          Sem.get_subst_map tenv pdesc params caller_mem callee_entry_mem ~callee_ret_alias
        in
        instantiate_ret ret callee_pname ~callee_entry_mem ~callee_exit_mem subst_map caller_mem
          ret_alias location
        |> instantiate_param tenv pdesc params callee_entry_mem callee_exit_mem subst_map location
    | None ->
        caller_mem


  let print_debug_info : Sil.instr -> Dom.Mem.astate -> Dom.Mem.astate -> unit =
   fun instr pre post ->
    L.(debug BufferOverrun Verbose) "@\n@\n================================@\n" ;
    L.(debug BufferOverrun Verbose) "@[<v 2>Pre-state : @,%a" Dom.Mem.pp pre ;
    L.(debug BufferOverrun Verbose) "@]@\n@\n%a" (Sil.pp_instr Pp.text) instr ;
    L.(debug BufferOverrun Verbose) "@\n@\n" ;
    L.(debug BufferOverrun Verbose) "@[<v 2>Post-state : @,%a" Dom.Mem.pp post ;
    L.(debug BufferOverrun Verbose) "@]@\n" ;
    L.(debug BufferOverrun Verbose) "================================@\n@."


  let exec_instr : Dom.Mem.astate -> extras ProcData.t -> CFG.node -> Sil.instr -> Dom.Mem.astate =
   fun mem {pdesc; tenv} node instr ->
    let pname = Procdesc.get_proc_name pdesc in
    let output_mem =
      match instr with
      | Load (id, _, _, _) when Ident.is_none id ->
          mem
      | Load (id, exp, _, _) ->
          BoUtils.Exec.load_val id (Sem.eval exp mem) mem
      | Store (exp1, _, exp2, location) ->
          let locs = Sem.eval exp1 mem |> Dom.Val.get_all_locs in
          let v = Sem.eval exp2 mem |> Dom.Val.add_trace_elem (Trace.Assign location) in
          let mem = Dom.Mem.update_mem locs v mem in
          let mem =
            if PowLoc.is_singleton locs then
              let loc_v = PowLoc.min_elt locs in
              match Typ.Procname.get_method pname with
              | "__inferbo_empty" when Loc.is_return loc_v -> (
                match Sem.get_formals pdesc with
                | [(formal, _)] ->
                    let formal_v = Dom.Mem.find_heap (Loc.of_pvar formal) mem in
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
      | Call (ret, Const Cfun callee_pname, params, location, _) -> (
        match Models.Call.dispatch callee_pname params with
        | Some {Models.exec} ->
            let node_hash = CFG.hash node in
            let model_env = Models.mk_model_env callee_pname node_hash location tenv ?ret in
            exec model_env mem
        | None ->
          match Summary.read_summary pdesc callee_pname with
          | Some summary ->
              let callee = Ondemand.get_proc_desc callee_pname in
              instantiate_mem tenv ret callee callee_pname params mem summary location
          | None ->
              L.(debug BufferOverrun Verbose)
                "/!\\ Unknown call to %a at %a@\n" Typ.Procname.pp callee_pname Location.pp
                location ;
              let val_unknown = Dom.Val.unknown_from ~callee_pname ~location in
              Models.model_by_value val_unknown ret mem |> Dom.Mem.add_heap Loc.unknown val_unknown
        )
      | Declare_locals (locals, location) ->
          (* array allocation in stack e.g., int arr[10] *)
          let node_hash = CFG.hash node in
          let rec decl_local pname ~node_hash location loc typ ~inst_num ~dimension mem =
            match typ.Typ.desc with
            | Typ.Tarray {elt= typ; length; stride} ->
                let stride = Option.map ~f:IntLit.to_int stride in
                BoUtils.Exec.decl_local_array ~decl_local pname ~node_hash location loc typ ~length
                  ?stride ~inst_num ~dimension mem
            | Typ.Tstruct typname -> (
              match Models.TypName.dispatch typname with
              | Some {Models.declare_local} ->
                  let model_env = Models.mk_model_env pname node_hash location tenv in
                  declare_local ~decl_local model_env loc ~inst_num ~dimension mem
              | None ->
                  (mem, inst_num) )
            | _ ->
                (mem, inst_num)
          in
          let try_decl_local (mem, inst_num) (pvar, typ) =
            let loc = Loc.of_pvar pvar in
            decl_local pname ~node_hash location loc typ ~inst_num ~dimension:1 mem
          in
          let mem, inst_num = List.fold ~f:try_decl_local ~init:(mem, 1) locals in
          let formals = Sem.get_formals pdesc in
          declare_symbolic_parameters pname tenv ~node_hash location ~inst_num formals mem
      | Call (_, fun_exp, _, location, _) ->
          let () =
            L.(debug BufferOverrun Verbose)
              "/!\\ Call to non-const function %a at %a" Exp.pp fun_exp Location.pp location
          in
          mem
      | Remove_temps (temps, _) ->
          Dom.Mem.remove_temps temps mem
      | Abstract _ | Nullify _ ->
          mem
    in
    print_debug_info instr mem output_mem ;
    output_mem
end

module Analyzer = AbstractInterpreter.Make (ProcCfg.Normal) (TransferFunctions)
module CFG = Analyzer.TransferFunctions.CFG

module Report = struct
  module PO = BufferOverrunProofObligations

  type extras = ProcData.no_extras

  module ExitStatement = struct
    let non_significant_instr = function
      | Sil.(Abstract _ | Nullify _ | Remove_temps _) ->
          true
      | _ ->
          false


    (* check that we are the last significant instruction
     * of a procedure (no more significant instruction)
     * or of a block (goes directly to a node with multiple predecessors)
     *)
    let rec is_end_of_block_or_procedure node rem_instrs =
      List.for_all rem_instrs ~f:non_significant_instr
      &&
      match Procdesc.Node.get_succs node with
      | [] ->
          true
      | [succ]
        -> (
          is_end_of_block_or_procedure succ (CFG.instrs succ)
          ||
          match Procdesc.Node.get_preds succ with
          | _ :: _ :: _ ->
              true (* [succ] is a join, i.e. [node] is the end of a block *)
          | _ ->
              false )
      | _ :: _ :: _ ->
          false
  end

  let check_unreachable_code summary tenv node instr rem_instrs mem =
    match mem with
    | NonBottom _ ->
        ()
    | Bottom ->
      match instr with
      | Sil.Prune (_, _, _, (Ik_land_lor | Ik_bexp)) ->
          ()
      | Sil.Prune (cond, location, true_branch, _) ->
          let i = match cond with Exp.Const Const.Cint i -> i | _ -> IntLit.zero in
          let desc = Errdesc.explain_condition_always_true_false tenv i cond node location in
          let exn = Exceptions.Condition_always_true_false (desc, not true_branch, __POS__) in
          Reporting.log_warning summary ~loc:location exn
      (* special case for `exit` when we're at the end of a block / procedure *)
      | Sil.Call (_, Const Cfun pname, _, _, _)
        when String.equal (Typ.Procname.get_method pname) "exit"
             && ExitStatement.is_end_of_block_or_procedure node rem_instrs ->
          ()
      | _ ->
          let location = Sil.instr_get_loc instr in
          let desc = Errdesc.explain_unreachable_code_after location in
          let exn = Exceptions.Unreachable_code_after (desc, __POS__) in
          Reporting.log_error summary ~loc:location exn


  let check_binop_array_access
      : Typ.Procname.t -> is_plus:bool -> e1:Exp.t -> e2:Exp.t -> Location.t -> Dom.Mem.astate
        -> PO.ConditionSet.t -> PO.ConditionSet.t =
   fun pname ~is_plus ~e1 ~e2 location mem cond_set ->
    let arr = Sem.eval e1 mem in
    let idx = Sem.eval e2 mem in
    BoUtils.Check.array_access ~arr ~idx ~is_plus pname location cond_set


  let check_binop
      : Typ.Procname.t -> bop:Binop.t -> e1:Exp.t -> e2:Exp.t -> Location.t -> Dom.Mem.astate
        -> PO.ConditionSet.t -> PO.ConditionSet.t =
   fun pname ~bop ~e1 ~e2 location mem cond_set ->
    match bop with
    | Binop.PlusPI ->
        check_binop_array_access pname ~is_plus:true ~e1 ~e2 location mem cond_set
    | Binop.MinusPI ->
        check_binop_array_access pname ~is_plus:false ~e1 ~e2 location mem cond_set
    | _ ->
        cond_set


  let check_expr
      : Typ.Procname.t -> Exp.t -> Location.t -> Dom.Mem.astate -> PO.ConditionSet.t
        -> PO.ConditionSet.t =
   fun pname exp location mem cond_set ->
    let rec check_sub_expr exp cond_set =
      match exp with
      | Exp.Lindex (array_exp, index_exp) ->
          cond_set |> check_sub_expr array_exp |> check_sub_expr index_exp
          |> BoUtils.Check.lindex ~array_exp ~index_exp mem pname location
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
        BoUtils.Check.array_access ~arr ~idx:Dom.Val.Itv.zero ~is_plus:true pname location cond_set
    | Exp.BinOp (bop, e1, e2) ->
        check_binop pname ~bop ~e1 ~e2 location mem cond_set
    | _ ->
        cond_set


  let instantiate_cond
      : Tenv.t -> Typ.Procname.t -> Procdesc.t option -> (Exp.t * Typ.t) list -> Dom.Mem.astate
        -> Summary.payload -> Location.t -> PO.ConditionSet.t =
   fun tenv caller_pname callee_pdesc params caller_mem summary location ->
    let callee_entry_mem = Dom.Summary.get_input summary in
    let callee_cond = Dom.Summary.get_cond_set summary in
    match callee_pdesc with
    | Some pdesc ->
        let subst_map, _ =
          Sem.get_subst_map tenv pdesc params caller_mem callee_entry_mem ~callee_ret_alias:None
        in
        let pname = Procdesc.get_proc_name pdesc in
        PO.ConditionSet.subst callee_cond subst_map caller_pname pname location
    | _ ->
        callee_cond


  let check_instr
      : extras ProcData.t -> CFG.node -> Sil.instr -> Dom.Mem.astate -> PO.ConditionSet.t
        -> PO.ConditionSet.t =
   fun {pdesc; tenv} node instr mem cond_set ->
    let pname = Procdesc.get_proc_name pdesc in
    match instr with
    | Sil.Load (_, exp, _, location) | Sil.Store (exp, _, _, location) ->
        check_expr pname exp location mem cond_set
    | Sil.Call (_, Const Cfun callee_pname, params, location, _) -> (
      match Models.Call.dispatch callee_pname params with
      | Some {Models.check} ->
          let node_hash = CFG.hash node in
          check (Models.mk_model_env pname node_hash location tenv) mem cond_set
      | None ->
        match Summary.read_summary pdesc callee_pname with
        | Some callee_summary ->
            let callee = Ondemand.get_proc_desc callee_pname in
            instantiate_cond tenv pname callee params mem callee_summary location
            |> PO.ConditionSet.join cond_set
        | _ ->
            cond_set )
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


  let rec check_instrs
      : Specs.summary -> extras ProcData.t -> CFG.node -> Sil.instr list -> Dom.Mem.astate
        -> PO.ConditionSet.t -> PO.ConditionSet.t =
   fun summary ({tenv} as pdata) node instrs mem cond_set ->
    match (mem, instrs) with
    | _, [] | Bottom, _ ->
        cond_set
    | NonBottom _, instr :: rem_instrs ->
        let cond_set = check_instr pdata node instr mem cond_set in
        let mem = Analyzer.TransferFunctions.exec_instr mem pdata node instr in
        let () = check_unreachable_code summary tenv node instr rem_instrs mem in
        print_debug_info instr mem cond_set ;
        check_instrs summary pdata node rem_instrs mem cond_set


  let check_node
      : Specs.summary -> extras ProcData.t -> Analyzer.invariant_map -> PO.ConditionSet.t
        -> CFG.node -> PO.ConditionSet.t =
   fun summary pdata inv_map cond_set node ->
    match Analyzer.extract_pre (CFG.id node) inv_map with
    | Some mem ->
        let instrs = CFG.instrs node in
        check_instrs summary pdata node instrs mem cond_set
    | _ ->
        cond_set


  let check_proc
      : Specs.summary -> extras ProcData.t -> Analyzer.invariant_map -> PO.ConditionSet.t =
   fun summary ({pdesc} as pdata) inv_map ->
    Procdesc.fold_nodes pdesc ~f:(check_node summary pdata inv_map) ~init:PO.ConditionSet.empty


  let make_err_trace : Trace.t -> string -> Errlog.loc_trace =
   fun trace issue_desc ->
    let f elem (trace, depth) =
      match elem with
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
      | Trace.UnknownFrom (pname, location) ->
          let desc = Format.asprintf "Unknown value from: %a" Typ.Procname.pp pname in
          (Errlog.make_trace_element depth location desc [] :: trace, depth)
    in
    List.fold_right ~f ~init:([], 0) trace.trace |> fst |> List.rev


  let report_errors : Specs.summary -> Procdesc.t -> PO.ConditionSet.t -> PO.ConditionSet.t =
   fun summary pdesc cond_set ->
    let pname = Procdesc.get_proc_name pdesc in
    let report cond trace issue_type =
      let caller_pname, location =
        match PO.ConditionTrace.get_cond_trace trace with
        | PO.ConditionTrace.Inter (caller_pname, _, location) ->
            (caller_pname, location)
        | PO.ConditionTrace.Intra pname ->
            (pname, PO.ConditionTrace.get_location trace)
      in
      if Typ.Procname.equal pname caller_pname then
        let description = PO.description cond trace in
        let error_desc = Localise.desc_buffer_overrun description in
        let exn = Exceptions.Checkers (issue_type, error_desc) in
        let trace =
          match TraceSet.choose_shortest trace.PO.ConditionTrace.val_traces with
          | trace ->
              make_err_trace trace description
          | exception _ ->
              [Errlog.make_trace_element 0 location description []]
        in
        Reporting.log_error summary ~loc:location ~ltr:trace exn
    in
    PO.ConditionSet.check_all ~report cond_set
end

let extract_post inv_map node =
  let id = CFG.id node in
  Analyzer.extract_post id inv_map


let compute_post
    : Specs.summary -> Analyzer.TransferFunctions.extras ProcData.t -> Summary.payload option =
 fun summary ({pdesc} as pdata) ->
  let cfg = CFG.from_pdesc pdesc in
  let inv_map = Analyzer.exec_pdesc ~initial:Dom.Mem.init pdata in
  let entry_mem = extract_post inv_map (CFG.start_node cfg) in
  let exit_mem = extract_post inv_map (CFG.exit_node cfg) in
  let cond_set = Report.check_proc summary pdata inv_map |> Report.report_errors summary pdesc in
  match (entry_mem, exit_mem) with
  | Some entry_mem, Some exit_mem ->
      Some (entry_mem, exit_mem, cond_set)
  | _ ->
      None


let print_summary : Typ.Procname.t -> Dom.Summary.t -> unit =
 fun proc_name s ->
  L.(debug BufferOverrun Medium)
    "@\n@[<v 2>Summary of %a :@,%a@]@." Typ.Procname.pp proc_name Dom.Summary.pp_summary s


let checker : Callbacks.proc_callback_args -> Specs.summary =
 fun {proc_desc; tenv; summary} ->
  Preanal.do_preanalysis proc_desc tenv ;
  let proc_data = ProcData.make_default proc_desc tenv in
  match compute_post summary proc_data with
  | Some post ->
      ( if Config.bo_debug >= 1 then
          let proc_name = Specs.get_proc_name summary in
          print_summary proc_name post ) ;
      Summary.update_summary post summary
  | None ->
      summary
