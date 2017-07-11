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
module F = Format
module L = Logging
module Dom = BufferOverrunDomain
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
  module Sem = BufferOverrunSemantics.Make (CFG)

  type extras = Typ.Procname.t -> Procdesc.t option

  let set_uninitialized node (typ: Typ.t) loc mem =
    match typ.desc with
    | Tint _ | Tfloat _
     -> Dom.Mem.weak_update_heap loc Dom.Val.Itv.top mem
    | _
     -> L.(debug BufferOverrun Verbose)
          "/!\\ Do not know how to uninitialize type %a at %a@\n" (Typ.pp Pp.text) typ Location.pp
          (CFG.loc node) ;
        mem

  (* NOTE: heuristic *)
  let get_malloc_info : Exp.t -> Typ.t * Int.t option * Exp.t = function
    | Exp.BinOp (Binop.Mult, Exp.Sizeof {typ; nbytes}, length)
    | Exp.BinOp (Binop.Mult, length, Exp.Sizeof {typ; nbytes})
     -> (typ, nbytes, length)
    | Exp.Sizeof {typ; nbytes}
     -> (typ, nbytes, Exp.one)
    | x
     -> (Typ.mk (Typ.Tint Typ.IChar), Some 1, x)

  let model_malloc
      : Typ.Procname.t -> (Ident.t * Typ.t) option -> (Exp.t * Typ.t) list -> CFG.node
        -> Location.t -> Dom.Mem.astate -> Dom.Mem.astate =
    fun pname ret params node location mem ->
      match ret with
      | Some (id, _)
       -> let typ, stride, length0 = get_malloc_info (List.hd_exn params |> fst) in
          let length = Sem.eval length0 mem (CFG.loc node) in
          let traces = TraceSet.add_elem (Trace.ArrDecl location) (Dom.Val.get_traces length) in
          let v =
            Sem.eval_array_alloc pname node typ ?stride Itv.zero (Dom.Val.get_itv length) 0 1
            |> Dom.Val.set_traces traces
          in
          mem |> Dom.Mem.add_stack (Loc.of_id id) v
          |> set_uninitialized node typ (Dom.Val.get_array_locs v)
      | _
       -> L.(debug BufferOverrun Verbose)
            "/!\\ Do not know where to model malloc at %a@\n" Location.pp (CFG.loc node) ;
          mem

  let model_realloc
      : Typ.Procname.t -> (Ident.t * Typ.t) option -> (Exp.t * Typ.t) list -> CFG.node
        -> Location.t -> Dom.Mem.astate -> Dom.Mem.astate =
    fun pname ret params node location mem ->
      model_malloc pname ret (List.tl_exn params) node location mem

  let model_by_value value ret mem =
    match ret with
    | Some (id, _)
     -> Dom.Mem.add_stack (Loc.of_id id) value mem
    | None
     -> L.(debug BufferOverrun Verbose)
          "/!\\ Do not know where to model value %a@\n" Dom.Val.pp value ;
        mem

  let model_infer_print : (Exp.t * Typ.t) list -> Dom.Mem.astate -> Location.t -> Dom.Mem.astate =
    fun params mem loc ->
      match params with
      | (e, _) :: _
       -> L.(debug BufferOverrun Medium)
            "@[<v>=== Infer Print === at %a@,%a@]%!" Location.pp loc Dom.Val.pp
            (Sem.eval e mem loc) ;
          mem
      | _
       -> mem

  let model_infer_set_array_length pname node params mem loc =
    match params with
    | [(Exp.Lvar array_pvar, {Typ.desc= Typ.Tarray (typ, _, stride0)}); (length_exp, _)]
     -> let length = Sem.eval length_exp mem loc |> Dom.Val.get_itv in
        let stride = Option.map ~f:IntLit.to_int stride0 in
        let v = Sem.eval_array_alloc pname node typ ?stride Itv.zero length 0 1 in
        mem |> Dom.Mem.add_stack (Loc.of_pvar array_pvar) v
        |> set_uninitialized node typ (Dom.Val.get_array_locs v)
    | [_; _]
     -> failwithf "Unexpected type of arguments for __set_array_length()"
    | _
     -> failwithf "Unexpected number of arguments for __set_array_length()"

  let handle_unknown_call
      : Typ.Procname.t -> (Ident.t * Typ.t) option -> Typ.Procname.t -> (Exp.t * Typ.t) list
        -> CFG.node -> Dom.Mem.astate -> Location.t -> Dom.Mem.astate =
    fun pname ret callee_pname params node mem loc ->
      match Typ.Procname.get_method callee_pname with
      | "__exit" | "exit"
       -> Dom.Mem.Bottom
      | "fgetc"
       -> model_by_value Dom.Val.Itv.m1_255 ret mem
      | "infer_print"
       -> model_infer_print params mem loc
      | "malloc" | "__new_array"
       -> model_malloc pname ret params node loc mem
      | "realloc"
       -> model_realloc pname ret params node loc mem
      | "__set_array_length"
       -> model_infer_set_array_length pname node params mem loc
      | "strlen"
       -> model_by_value Dom.Val.Itv.nat ret mem
      | proc_name
       -> L.(debug BufferOverrun Verbose)
            "/!\\ Unknown call to %s at %a@\n" proc_name Location.pp loc ;
          model_by_value Dom.Val.unknown ret mem |> Dom.Mem.add_heap Loc.unknown Dom.Val.unknown

  let rec declare_array
      : Typ.Procname.t -> CFG.node -> Location.t -> Loc.t -> Typ.t -> length:IntLit.t option
        -> ?stride:int -> inst_num:int -> dimension:int -> Dom.Mem.astate -> Dom.Mem.astate =
    fun pname node location loc typ ~length ?stride ~inst_num ~dimension mem ->
      let size = Option.value_map ~default:Itv.top ~f:Itv.of_int_lit length in
      let arr =
        Sem.eval_array_alloc pname node typ Itv.zero size ?stride inst_num dimension
        |> Dom.Val.add_trace_elem (Trace.ArrDecl location)
      in
      let mem =
        if Int.equal dimension 1 then Dom.Mem.add_stack loc arr mem
        else Dom.Mem.add_heap loc arr mem
      in
      let loc = Loc.of_allocsite (Sem.get_allocsite pname node inst_num dimension) in
      match typ.Typ.desc with
      | Typ.Tarray (typ, length, stride)
       -> declare_array pname node location loc typ ~length
            ?stride:(Option.map ~f:IntLit.to_int stride) ~inst_num ~dimension:(dimension + 1) mem
      | _
       -> mem

  let counter_gen init =
    let num_ref = ref init in
    let get_num () =
      let v = !num_ref in
      num_ref := v + 1 ;
      v
    in
    get_num

  let declare_symbolic_val
      : Typ.Procname.t -> Tenv.t -> CFG.node -> Location.t -> Loc.t -> Typ.typ -> inst_num:int
        -> new_sym_num:(unit -> int) -> Domain.t -> Domain.t =
    fun pname tenv node location loc typ ~inst_num ~new_sym_num mem ->
      let max_depth = 2 in
      let new_alloc_num = counter_gen 1 in
      let rec decl_sym_val ~depth loc typ mem =
        if depth > max_depth then mem
        else
          let depth = depth + 1 in
          match typ.Typ.desc with
          | Typ.Tint ikind
           -> let unsigned = Typ.ikind_is_unsigned ikind in
              let v =
                Dom.Val.make_sym ~unsigned pname new_sym_num
                |> Dom.Val.add_trace_elem (Trace.SymAssign location)
              in
              Dom.Mem.add_heap loc v mem
          | Typ.Tfloat _
           -> let v =
                Dom.Val.make_sym pname new_sym_num
                |> Dom.Val.add_trace_elem (Trace.SymAssign location)
              in
              Dom.Mem.add_heap loc v mem
          | Typ.Tptr (typ, _)
           -> decl_sym_arr ~depth loc location typ mem
          | Typ.Tarray (typ, opt_int_lit, _)
           -> let opt_size = Option.map ~f:Itv.of_int_lit opt_int_lit in
              let opt_offset = Some Itv.zero in
              decl_sym_arr ~depth loc location typ ~opt_offset ~opt_size mem
          | Typ.Tstruct typename
           -> let decl_fld mem (fn, typ, _) =
                let loc_fld = Loc.append_field loc fn in
                decl_sym_val ~depth loc_fld typ mem
              in
              let decl_flds str = List.fold ~f:decl_fld ~init:mem str.Typ.Struct.fields in
              let opt_struct = Tenv.lookup tenv typename in
              Option.value_map opt_struct ~default:mem ~f:decl_flds
          | _
           -> if Config.bo_debug >= 3 then
                L.(debug BufferOverrun Verbose)
                  "/!\\ decl_fld of unhandled type: %a at %a@." (Typ.pp Pp.text) typ Location.pp
                  (CFG.loc node) ;
              mem
      and decl_sym_arr ~depth loc location typ ?(opt_offset= None) ?(opt_size= None) mem =
        let option_value opt_x default_f = match opt_x with Some x -> x | None -> default_f () in
        let itv_make_sym () = Itv.make_sym pname new_sym_num in
        let offset = option_value opt_offset itv_make_sym in
        let size = option_value opt_size itv_make_sym in
        let alloc_num = new_alloc_num () in
        let elem = Trace.SymAssign location in
        let arr =
          Sem.eval_array_alloc pname node typ offset size inst_num alloc_num
          |> Dom.Val.add_trace_elem elem
        in
        let mem = Dom.Mem.add_heap loc arr mem in
        let deref_loc = Loc.of_allocsite (Sem.get_allocsite pname node inst_num alloc_num) in
        decl_sym_val ~depth deref_loc typ mem
      in
      decl_sym_val ~depth:0 loc typ mem

  let declare_symbolic_parameter
      : Procdesc.t -> Tenv.t -> CFG.node -> Location.t -> int -> Dom.Mem.astate -> Dom.Mem.astate =
    fun pdesc tenv node location inst_num mem ->
      let pname = Procdesc.get_proc_name pdesc in
      let new_sym_num = counter_gen 0 in
      let add_formal (mem, inst_num) (pvar, typ) =
        let loc = Loc.of_pvar pvar in
        let mem =
          declare_symbolic_val pname tenv node location loc typ ~inst_num ~new_sym_num mem
        in
        (mem, inst_num + 1)
      in
      List.fold ~f:add_formal ~init:(mem, inst_num) (Sem.get_formals pdesc) |> fst

  let instantiate_ret ret callee_pname callee_exit_mem subst_map mem loc =
    match ret with
    | Some (id, _)
     -> let ret_loc = Loc.of_pvar (Pvar.get_ret_pvar callee_pname) in
        let ret_val = Dom.Mem.find_heap ret_loc callee_exit_mem in
        let ret_var = Loc.of_var (Var.of_id id) in
        Dom.Val.subst ret_val subst_map loc |> Dom.Val.add_trace_elem (Trace.Return loc)
        |> Fn.flip (Dom.Mem.add_stack ret_var) mem
    | None
     -> mem

  let instantiate_param tenv pdesc params callee_entry_mem callee_exit_mem subst_map location mem =
    let formals = Sem.get_formals pdesc in
    let actuals = List.map ~f:(fun (a, _) -> Sem.eval a mem location) params in
    let f mem formal actual =
      match (snd formal).Typ.desc with
      | Typ.Tptr (typ, _) -> (
        match typ.Typ.desc with
        | Typ.Tstruct typename -> (
          match Tenv.lookup tenv typename with
          | Some str
           -> let formal_locs =
                Dom.Mem.find_heap (Loc.of_pvar (fst formal)) callee_entry_mem
                |> Dom.Val.get_array_blk |> ArrayBlk.get_pow_loc
              in
              let instantiate_fld mem (fn, _, _) =
                let formal_fields = PowLoc.append_field formal_locs fn in
                let v = Dom.Mem.find_heap_set formal_fields callee_exit_mem in
                let actual_fields = PowLoc.append_field (Dom.Val.get_all_locs actual) fn in
                Dom.Val.subst v subst_map location
                |> Fn.flip (Dom.Mem.strong_update_heap actual_fields) mem
              in
              List.fold ~f:instantiate_fld ~init:mem str.Typ.Struct.fields
          | _
           -> mem )
        | _
         -> let formal_locs =
              Dom.Mem.find_heap (Loc.of_pvar (fst formal)) callee_entry_mem
              |> Dom.Val.get_array_blk |> ArrayBlk.get_pow_loc
            in
            let v = Dom.Mem.find_heap_set formal_locs callee_exit_mem in
            let actual_locs = Dom.Val.get_all_locs actual in
            Dom.Val.subst v subst_map location
            |> Fn.flip (Dom.Mem.strong_update_heap actual_locs) mem )
      | _
       -> mem
    in
    try List.fold2_exn formals actuals ~init:mem ~f
    with Invalid_argument _ -> mem

  let instantiate_mem
      : Tenv.t -> (Ident.t * Typ.t) option -> Procdesc.t option -> Typ.Procname.t
        -> (Exp.t * Typ.t) list -> Dom.Mem.astate -> Dom.Summary.t -> Location.t -> Dom.Mem.astate =
    fun tenv ret callee_pdesc callee_pname params caller_mem summary loc ->
      let callee_entry_mem = Dom.Summary.get_input summary in
      let callee_exit_mem = Dom.Summary.get_output summary in
      match callee_pdesc with
      | Some pdesc
       -> let subst_map = Sem.get_subst_map tenv pdesc params caller_mem callee_entry_mem loc in
          instantiate_ret ret callee_pname callee_exit_mem subst_map caller_mem loc
          |> instantiate_param tenv pdesc params callee_entry_mem callee_exit_mem subst_map loc
      | None
       -> caller_mem

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
    fun mem {pdesc; tenv; extras} node instr ->
      let pname = Procdesc.get_proc_name pdesc in
      let output_mem =
        match instr with
        | Load (id, exp, _, loc)
         -> let locs = Sem.eval exp mem loc |> Dom.Val.get_all_locs in
            let v = Dom.Mem.find_heap_set locs mem in
            if Ident.is_none id then mem
            else Dom.Mem.add_stack (Loc.of_var (Var.of_id id)) v mem |> Dom.Mem.load_alias id exp
        | Store (exp1, _, exp2, loc)
         -> let locs = Sem.eval exp1 mem loc |> Dom.Val.get_all_locs in
            let v = Sem.eval exp2 mem loc |> Dom.Val.add_trace_elem (Trace.Assign loc) in
            Dom.Mem.update_mem locs v mem |> Dom.Mem.store_alias exp1 exp2
        | Prune (exp, loc, _, _)
         -> Sem.prune exp loc mem
        | Call (ret, Const Cfun callee_pname, params, loc, _) -> (
          match Summary.read_summary pdesc callee_pname with
          | Some summary
           -> let callee = extras callee_pname in
              instantiate_mem tenv ret callee callee_pname params mem summary loc
          | None
           -> handle_unknown_call pname ret callee_pname params node mem loc )
        | Declare_locals (locals, location)
         -> (* array allocation in stack e.g., int arr[10] *)
            let try_decl_arr location (mem, inst_num) (pvar, typ) =
              match typ.Typ.desc with
              | Typ.Tarray (typ, length, stride0)
               -> let loc = Loc.of_pvar pvar in
                  let stride = Option.map ~f:IntLit.to_int stride0 in
                  let mem =
                    declare_array pname node location loc typ ~length ?stride ~inst_num
                      ~dimension:1 mem
                  in
                  (mem, inst_num + 1)
              | _
               -> (mem, inst_num)
            in
            let mem, inst_num = List.fold ~f:(try_decl_arr location) ~init:(mem, 1) locals in
            declare_symbolic_parameter pdesc tenv node location inst_num mem
        | Call (_, fun_exp, _, loc, _)
         -> let () =
              L.(debug BufferOverrun Verbose)
                "/!\\ Call to non-const function %a at %a" Exp.pp fun_exp Location.pp loc
            in
            mem
        | Remove_temps _ | Abstract _ | Nullify _
         -> mem
      in
      print_debug_info instr mem output_mem ; output_mem
end

module Analyzer = AbstractInterpreter.Make (ProcCfg.Normal) (TransferFunctions)
module CFG = Analyzer.TransferFunctions.CFG
module Sem = BufferOverrunSemantics.Make (CFG)

module Report = struct
  type extras = Typ.Procname.t -> Procdesc.t option

  let add_condition
      : Typ.Procname.t -> CFG.node -> Exp.t -> Location.t -> Dom.Mem.astate -> Dom.ConditionSet.t
        -> Dom.ConditionSet.t =
    fun pname node exp loc mem cond_set ->
      let array_access =
        match exp with
        | Exp.Var _
         -> let v = Sem.eval exp mem loc in
            let arr = Dom.Val.get_array_blk v in
            let arr_traces = Dom.Val.get_traces v in
            Some (arr, arr_traces, Itv.zero, TraceSet.empty, true)
        | Exp.Lindex (e1, e2)
         -> let locs = Sem.eval_locs e1 mem loc |> Dom.Val.get_all_locs in
            let v_arr = Dom.Mem.find_set locs mem in
            let arr = Dom.Val.get_array_blk v_arr in
            let arr_traces = Dom.Val.get_traces v_arr in
            let v_idx = Sem.eval e2 mem loc in
            let idx = Dom.Val.get_itv v_idx in
            let idx_traces = Dom.Val.get_traces v_idx in
            Some (arr, arr_traces, idx, idx_traces, true)
        | Exp.BinOp ((Binop.PlusA as bop), e1, e2) | Exp.BinOp ((Binop.MinusA as bop), e1, e2)
         -> let v_arr = Sem.eval e1 mem loc in
            let arr = Dom.Val.get_array_blk v_arr in
            let arr_traces = Dom.Val.get_traces v_arr in
            let v_idx = Sem.eval e2 mem loc in
            let idx = Dom.Val.get_itv v_idx in
            let idx_traces = Dom.Val.get_traces v_idx in
            let is_plus = Binop.equal bop Binop.PlusA in
            Some (arr, arr_traces, idx, idx_traces, is_plus)
        | _
         -> None
      in
      match array_access with
      | Some (arr, traces_arr, idx, traces_idx, is_plus)
       -> let site = Sem.get_allocsite pname node 0 0 in
          let size = ArrayBlk.sizeof arr in
          let offset = ArrayBlk.offsetof arr in
          let idx = (if is_plus then Itv.plus else Itv.minus) offset idx in
          L.(debug BufferOverrun Verbose) "@[<v 2>Add condition :@," ;
          L.(debug BufferOverrun Verbose) "array: %a@," ArrayBlk.pp arr ;
          L.(debug BufferOverrun Verbose) "  idx: %a@," Itv.pp idx ;
          L.(debug BufferOverrun Verbose) "@]@." ;
          if size <> Itv.bot && idx <> Itv.bot then
            let traces = TraceSet.merge ~traces_arr ~traces_idx loc in
            Dom.ConditionSet.add_bo_safety pname loc site ~size ~idx traces cond_set
          else cond_set
      | None
       -> cond_set

  let instantiate_cond
      : Tenv.t -> Typ.Procname.t -> Procdesc.t option -> (Exp.t * Typ.t) list -> Dom.Mem.astate
        -> Summary.payload -> Location.t -> Dom.ConditionSet.t =
    fun tenv caller_pname callee_pdesc params caller_mem summary loc ->
      let callee_entry_mem = Dom.Summary.get_input summary in
      let callee_cond = Dom.Summary.get_cond_set summary in
      match callee_pdesc with
      | Some pdesc
       -> let subst_map = Sem.get_subst_map tenv pdesc params caller_mem callee_entry_mem loc in
          let pname = Procdesc.get_proc_name pdesc in
          Dom.ConditionSet.subst callee_cond subst_map caller_pname pname loc
      | _
       -> callee_cond

  let print_debug_info : Sil.instr -> Dom.Mem.astate -> Dom.ConditionSet.t -> unit =
    fun instr pre cond_set ->
      L.(debug BufferOverrun Verbose) "@\n@\n================================@\n" ;
      L.(debug BufferOverrun Verbose) "@[<v 2>Pre-state : @,%a" Dom.Mem.pp pre ;
      L.(debug BufferOverrun Verbose) "@]@\n@\n%a" (Sil.pp_instr Pp.text) instr ;
      L.(debug BufferOverrun Verbose) "@[<v 2>@\n@\n%a" Dom.ConditionSet.pp cond_set ;
      L.(debug BufferOverrun Verbose) "@]@\n" ;
      L.(debug BufferOverrun Verbose) "================================@\n@."

  let is_last_statement_of_if_branch rem_instrs node =
    if rem_instrs <> [] then false
    else
      match Procdesc.Node.get_succs node with
      | [succ] -> (
        match Procdesc.Node.get_preds succ with _ :: _ :: _ -> true | _ -> false )
      | _
       -> false

  let rec collect_instrs
      : extras ProcData.t -> CFG.node -> Sil.instr list -> Dom.Mem.astate -> Dom.ConditionSet.t
        -> Dom.ConditionSet.t =
    fun ({pdesc; tenv; extras} as pdata) node instrs mem cond_set ->
      match instrs with
      | []
       -> cond_set
      | instr :: rem_instrs
       -> let pname = Procdesc.get_proc_name pdesc in
          let cond_set =
            match instr with
            | Sil.Load (_, exp, _, loc) | Sil.Store (exp, _, _, loc)
             -> add_condition pname node exp loc mem cond_set
            | Sil.Call (_, Const Cfun callee_pname, params, loc, _) -> (
              match Summary.read_summary pdesc callee_pname with
              | Some summary
               -> let callee = extras callee_pname in
                  instantiate_cond tenv pname callee params mem summary loc
                  |> Dom.ConditionSet.rm_invalid |> Dom.ConditionSet.join cond_set
              | _
               -> cond_set )
            | _
             -> cond_set
          in
          let mem' = Analyzer.TransferFunctions.exec_instr mem pdata node instr in
          let () =
            match (mem, mem') with
            | NonBottom _, Bottom -> (
              match instr with
              | Sil.Prune (_, _, _, (Ik_land_lor | Ik_bexp))
               -> ()
              | Sil.Prune (cond, loc, true_branch, _)
               -> let i = match cond with Exp.Const Const.Cint i -> i | _ -> IntLit.zero in
                  let desc = Errdesc.explain_condition_always_true_false tenv i cond node loc in
                  let exn =
                    Exceptions.Condition_always_true_false (desc, not true_branch, __POS__)
                  in
                  Reporting.log_warning_deprecated pname ~loc exn
              | Sil.Call (_, Const Cfun pname, _, _, _)
                when String.equal (Typ.Procname.get_method pname) "exit"
                     && is_last_statement_of_if_branch rem_instrs node
               -> ()
              | _
               -> let loc = Sil.instr_get_loc instr in
                  let desc = Errdesc.explain_unreachable_code_after loc in
                  let exn = Exceptions.Unreachable_code_after (desc, __POS__) in
                  Reporting.log_error_deprecated pname ~loc exn )
            | _
             -> ()
          in
          print_debug_info instr mem' cond_set ; collect_instrs pdata node rem_instrs mem' cond_set

  let collect_node
      : extras ProcData.t -> Analyzer.invariant_map -> Dom.ConditionSet.t -> CFG.node
        -> Dom.ConditionSet.t =
    fun pdata inv_map cond_set node ->
      match Analyzer.extract_pre (CFG.id node) inv_map with
      | Some mem
       -> let instrs = CFG.instrs node in
          collect_instrs pdata node instrs mem cond_set
      | _
       -> cond_set

  let collect : extras ProcData.t -> Analyzer.invariant_map -> Dom.ConditionSet.t =
    fun ({pdesc} as pdata) inv_map ->
      let add_node1 acc node = collect_node pdata inv_map acc node in
      Procdesc.fold_nodes add_node1 Dom.ConditionSet.empty pdesc

  let make_err_trace : Trace.t -> string -> Errlog.loc_trace =
    fun trace desc ->
      let f elem (trace, depth) =
        match elem with
        | Trace.Assign loc
         -> (Errlog.make_trace_element depth loc "Assignment" [] :: trace, depth)
        | Trace.ArrDecl loc
         -> (Errlog.make_trace_element depth loc "ArrayDeclaration" [] :: trace, depth)
        | Trace.Call loc
         -> (Errlog.make_trace_element depth loc "Call" [] :: trace, depth + 1)
        | Trace.Return loc
         -> (Errlog.make_trace_element (depth - 1) loc "Return" [] :: trace, depth - 1)
        | Trace.SymAssign _
         -> (trace, depth)
        | Trace.ArrAccess loc
         -> (Errlog.make_trace_element depth loc ("ArrayAccess: " ^ desc) [] :: trace, depth)
      in
      List.fold_right ~f ~init:([], 0) trace.trace |> fst |> List.rev

  let report_error : Procdesc.t -> Dom.ConditionSet.t -> unit =
    fun pdesc conds ->
      let pname = Procdesc.get_proc_name pdesc in
      let report1 cond =
        let alarm = Dom.Condition.check cond in
        let caller_pname, loc =
          match Dom.Condition.get_cond_trace cond with
          | Dom.Condition.Inter (caller_pname, _, loc)
           -> (caller_pname, loc)
          | Dom.Condition.Intra pname
           -> (pname, Dom.Condition.get_location cond)
        in
        match alarm with
        | None
         -> ()
        | Some bucket when Typ.Procname.equal pname caller_pname
         -> let description = Dom.Condition.to_string cond in
            let error_desc = Localise.desc_buffer_overrun bucket description in
            let exn =
              Exceptions.Checkers (Localise.to_issue_id Localise.buffer_overrun, error_desc)
            in
            let trace =
              match TraceSet.choose_shortest cond.Dom.Condition.traces with
              | trace
               -> make_err_trace trace description
              | exception _
               -> [Errlog.make_trace_element 0 loc description []]
            in
            Reporting.log_error_deprecated pname ~loc ~ltr:trace exn
        | _
         -> ()
      in
      Dom.ConditionSet.iter report1 conds
end

let compute_post : Analyzer.TransferFunctions.extras ProcData.t -> Summary.payload option =
  fun {pdesc; tenv; extras= get_pdesc} ->
    let cfg = CFG.from_pdesc pdesc in
    let pdata = ProcData.make pdesc tenv get_pdesc in
    let inv_map = Analyzer.exec_pdesc ~initial:Dom.Mem.init pdata in
    let entry_mem =
      let entry_id = CFG.id (CFG.start_node cfg) in
      Analyzer.extract_post entry_id inv_map
    in
    let exit_mem =
      let exit_id = CFG.id (CFG.exit_node cfg) in
      Analyzer.extract_post exit_id inv_map
    in
    let cond_set = Report.collect pdata inv_map in
    Report.report_error pdesc cond_set ;
    match (entry_mem, exit_mem) with
    | Some entry_mem, Some exit_mem
     -> Some (entry_mem, exit_mem, cond_set)
    | _
     -> None

let print_summary : Typ.Procname.t -> Dom.Summary.t -> unit =
  fun proc_name s ->
    L.(debug BufferOverrun Medium)
      "@\n@[<v 2>Summary of %a :@,%a@]@." Typ.Procname.pp proc_name Dom.Summary.pp_summary s

let checker : Callbacks.proc_callback_args -> Specs.summary =
  fun {proc_desc; tenv; summary; get_proc_desc} ->
    let proc_name = Specs.get_proc_name summary in
    let proc_data = ProcData.make proc_desc tenv get_proc_desc in
    match compute_post proc_data with
    | Some post
     -> if Config.bo_debug >= 1 then print_summary proc_name post ;
        Summary.update_summary post summary
    | None
     -> summary
