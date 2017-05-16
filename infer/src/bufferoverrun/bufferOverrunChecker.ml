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

module Summary = Summary.Make (struct
    type payload = Dom.Summary.t

    let update_payload astate (summary : Specs.summary) =
      { summary with payload = { summary.payload with buffer_overrun = Some astate }}

    let read_payload (summary : Specs.summary) =
      summary.payload.buffer_overrun
  end)

module TransferFunctions (CFG : ProcCfg.S) =
struct
  module CFG = CFG
  module Domain = Dom.Mem
  module Sem = BufferOverrunSemantics.Make (CFG)

  type extras = Typ.Procname.t -> Procdesc.t option

  let set_uninitialized (typ : Typ.t) loc mem = match typ.desc with
    | Tint _ | Tfloat _ -> Dom.Mem.weak_update_heap loc Dom.Val.top_itv mem
    | _ -> mem

  (* NOTE: heuristic *)
  let get_malloc_info : Exp.t -> Typ.t * Int.t option * Exp.t
    = function
      | Exp.BinOp (Binop.Mult, Exp.Sizeof {typ; nbytes}, length)
      | Exp.BinOp (Binop.Mult, length, Exp.Sizeof {typ; nbytes}) -> (typ, nbytes, length)
      | Exp.Sizeof {typ; nbytes} -> (typ, nbytes, Exp.one)
      | x -> (Typ.mk (Typ.Tint Typ.IChar), Some 1, x)

  let model_malloc
    : Typ.Procname.t -> (Ident.t * Typ.t) option -> (Exp.t * Typ.t) list -> CFG.node
      -> Dom.Mem.astate -> Dom.Mem.astate
    = fun pname ret params node mem ->
      match ret with
      | Some (id, _) ->
          let (typ, stride, length0) = get_malloc_info (List.hd_exn params |> fst) in
          let length = Sem.eval length0 mem (CFG.loc node) |> Dom.Val.get_itv in
          let v = Sem.eval_array_alloc pname node typ ?stride Itv.zero length 0 1 in
          mem
          |> Dom.Mem.add_stack (Loc.of_id id) v
          |> set_uninitialized typ (Dom.Val.get_array_locs v)
      | _ -> mem

  let model_realloc
    : Typ.Procname.t -> (Ident.t * Typ.t) option -> (Exp.t * Typ.t) list -> CFG.node
      -> Dom.Mem.astate -> Dom.Mem.astate
    = fun pname ret params node mem ->
      model_malloc pname ret (List.tl_exn params) node mem

  let model_fgetc : (Ident.t * Typ.t) option -> Dom.Mem.astate -> Dom.Mem.astate
    = fun ret mem ->
      match ret with
      | Some (id, _) ->
          let itv = Itv.make (Itv.Bound.of_int (-1)) Itv.Bound.PInf in
          Dom.Mem.add_stack (Loc.of_id id) (Dom.Val.of_itv itv) mem
      | _ -> mem

  let model_natual_itv : (Ident.t * Typ.t) option -> Dom.Mem.astate -> Dom.Mem.astate
    = fun ret mem ->
      match ret with
      | Some (id, _) -> Dom.Mem.add_stack (Loc.of_id id) Dom.Val.nat_itv mem
      | _ -> mem

  let model_unknown_itv : (Ident.t * Typ.t) option -> Dom.Mem.astate -> Dom.Mem.astate
    = fun ret mem ->
      match ret with
        Some (id, _) -> Dom.Mem.add_stack (Loc.of_id id) Dom.Val.top_itv mem
      | None -> mem

  let model_infer_print
    : (Exp.t * Typ.t) list -> Dom.Mem.astate -> Location.t -> Dom.Mem.astate
    = fun params mem loc ->
      match params with
      | (e, _) :: _ ->
          if Config.bo_debug >= 1 then
            L.err "@[<v>=== Infer Print === at %a@,%a@]%!"
              Location.pp loc
              Dom.Val.pp (Sem.eval e mem loc);
          mem
      | _ -> mem

  let model_infer_set_array_length pname node params mem loc =
    match params with
    | (Exp.Lvar array_pvar, {Typ.desc=Typ.Tarray (typ, _, stride0)})
      :: (length_exp, _) :: [] ->
        let length = Sem.eval length_exp mem loc |> Dom.Val.get_itv in
        let stride = Option.map ~f:IntLit.to_int stride0 in
        let v = Sem.eval_array_alloc pname node typ ?stride Itv.zero length 0 1 in
        mem
        |> Dom.Mem.add_stack (Loc.of_pvar array_pvar) v
        |> set_uninitialized typ (Dom.Val.get_array_locs v)
    | _ :: _ :: [] ->
        failwithf "Unexpected type of arguments for __set_array_length()"
    | _ ->
        failwithf "Unexpected number of arguments for __set_array_length()"

  let handle_unknown_call
    : Typ.Procname.t -> (Ident.t * Typ.t) option -> Typ.Procname.t
      -> (Exp.t * Typ.t) list -> CFG.node -> Dom.Mem.astate -> Location.t
      -> Dom.Mem.astate
    = fun pname ret callee_pname params node mem loc ->
      match Typ.Procname.get_method callee_pname with
      | "malloc"
      | "__new_array" -> model_malloc pname ret params node mem
      | "realloc" -> model_realloc pname ret params node mem
      | "strlen" -> model_natual_itv ret mem
      | "fgetc" -> model_fgetc ret mem
      | "infer_print" -> model_infer_print params mem loc
      | "__set_array_length" -> model_infer_set_array_length pname node params mem loc
      | _ -> model_unknown_itv ret mem

  let rec declare_array
    : Typ.Procname.t -> CFG.node -> Loc.t -> Typ.t -> length:IntLit.t option -> ?stride:int
    -> inst_num:int -> dimension:int -> Dom.Mem.astate -> Dom.Mem.astate
    = fun pname node loc typ ~length ?stride ~inst_num ~dimension mem ->
      let size = Option.value_map ~default:Itv.top ~f:Itv.of_int_lit length in
      let arr =
        Sem.eval_array_alloc pname node typ Itv.zero size ?stride inst_num dimension
      in
      let mem =
        if Int.equal dimension 1
        then Dom.Mem.add_stack loc arr mem
        else Dom.Mem.add_heap loc arr mem
      in
      let loc =
        Loc.of_allocsite (Sem.get_allocsite pname node inst_num dimension)
      in
      match typ.Typ.desc with
      | Typ.Tarray (typ, length, stride) ->
          declare_array pname node loc typ ~length ?stride:(Option.map ~f:IntLit.to_int stride)
            ~inst_num ~dimension:(dimension + 1) mem
      | _ -> mem

  let declare_symbolic_array
    : Typ.Procname.t -> Tenv.t -> CFG.node -> Loc.t -> Typ.t -> inst_num:int
    -> sym_num:int -> dimension:int -> Dom.Mem.astate -> Dom.Mem.astate * int
    = fun pname tenv node loc typ ~inst_num ~sym_num ~dimension mem ->
      let offset = Itv.make_sym pname sym_num in
      let size = Itv.make_sym pname (sym_num + 2) in
      let arr =
        Sem.eval_array_alloc pname node typ offset size inst_num dimension
      in
      let elem_val = Dom.Val.make_sym pname (sym_num + 4) in
      let arr_loc = arr |> Dom.Val.get_array_blk |> ArrayBlk.get_pow_loc in
      let mem =
        mem
        |> Dom.Mem.add_heap loc arr
        |> Dom.Mem.strong_update_heap arr_loc elem_val
      in
      let decl_fld (mem, sym_num) (fn, typ, _) =
        let loc =
          mem |> Dom.Mem.find_heap loc |> Dom.Val.get_array_locs |> PowLoc.choose
        in
        let field = Loc.append_field loc fn in
        match typ.Typ.desc with
        | Typ.Tint _
        | Typ.Tfloat _ ->
            let v = Dom.Val.make_sym pname sym_num in
            (Dom.Mem.add_heap field v mem, sym_num + 2)
        | Typ.Tptr (typ, _) ->
            let offset = Itv.make_sym pname sym_num in
            let size = Itv.make_sym pname (sym_num + 2) in
            let v =
              Sem.eval_array_alloc pname node typ offset size inst_num dimension
            in
            (Dom.Mem.add_heap field v mem, sym_num + 4)
        | _ ->
            if Config.bo_debug >= 3 then
              L.err "decl_fld of unhandled type: %a@." (Typ.pp Pp.text) typ;
            (mem, sym_num)
      in
      match typ.Typ.desc with
      | Typ.Tstruct typename ->
          (match Tenv.lookup tenv typename with
           | Some str ->
               List.fold ~f:decl_fld ~init:(mem, sym_num + 6) str.Typ.Struct.fields
           | _ -> (mem, sym_num + 6))
      | _ -> (mem, sym_num + 6)

  let declare_symbolic_parameter
    : Procdesc.t -> Tenv.t -> CFG.node -> int -> Dom.Mem.astate -> Dom.Mem.astate
    = fun pdesc tenv node inst_num mem ->
      let pname = Procdesc.get_proc_name pdesc in
      let add_formal (mem, inst_num, sym_num) (pvar, typ) =
        match typ.Typ.desc with
        | Typ.Tint _ ->
            let v = Dom.Val.make_sym pname sym_num in
            let mem = Dom.Mem.add_heap (Loc.of_pvar pvar) v mem in
            (mem, inst_num + 1, sym_num + 2)
        | Typ.Tptr (typ, _) ->
            let (mem, sym_num) =
              declare_symbolic_array pname tenv node (Loc.of_pvar pvar) typ
                ~inst_num ~sym_num ~dimension:1 mem
            in
            (mem, inst_num + 1, sym_num)
        | _ ->
            if Config.bo_debug >= 3 then
              L.err "declare_symbolic_parameter of unhandled type: %a@." (Typ.pp Pp.text) typ;
            (mem, inst_num, sym_num) (* TODO: add other cases if necessary *)
      in
      List.fold ~f:add_formal ~init:(mem, inst_num, 0) (Sem.get_formals pdesc)
      |> fst3

  let instantiate_ret
    : Tenv.t -> Procdesc.t option -> Typ.Procname.t -> (Exp.t * Typ.t) list
      -> Dom.Mem.astate -> Dom.Summary.t -> Location.t -> Dom.Val.astate
    = fun tenv callee_pdesc callee_pname params caller_mem summary loc ->
      let callee_entry_mem = Dom.Summary.get_input summary in
      let callee_exit_mem = Dom.Summary.get_output summary in
      match callee_pdesc with
      | Some pdesc ->
          let subst_map =
            Sem.get_subst_map tenv pdesc params caller_mem callee_entry_mem loc
          in
          let ret_loc = Loc.of_pvar (Pvar.get_ret_pvar callee_pname) in
          let ret_val = Dom.Mem.find_heap ret_loc callee_exit_mem in
          Dom.Val.subst ret_val subst_map
          |> Dom.Val.normalize    (* normalize bottom *)
      | _ -> Dom.Val.top_itv

  let print_debug_info : Sil.instr -> Dom.Mem.astate -> Dom.Mem.astate -> unit
    = fun instr pre post ->
      if Config.bo_debug >= 2 then
        begin
          L.err "@\n@\n================================@\n";
          L.err "@[<v 2>Pre-state : @,%a" Dom.Mem.pp pre;
          L.err "@]@\n@\n%a" (Sil.pp_instr Pp.text) instr;
          L.err "@\n@\n";
          L.err "@[<v 2>Post-state : @,%a" Dom.Mem.pp post;
          L.err "@]@\n";
          L.err "================================@\n@."
        end

  let exec_instr
    : Dom.Mem.astate -> extras ProcData.t -> CFG.node -> Sil.instr -> Dom.Mem.astate
    = fun mem { pdesc; tenv; extras } node instr ->
      let pname = Procdesc.get_proc_name pdesc in
      let try_decl_arr (mem, inst_num) (pvar, typ) =
        match typ.Typ.desc with
        | Typ.Tarray (typ, length, stride0) ->
            let loc = Loc.of_pvar pvar in
            let stride = Option.map ~f:IntLit.to_int stride0 in
            let mem =
              declare_array pname node loc typ ~length ?stride ~inst_num ~dimension:1 mem
            in
            (mem, inst_num + 1)
        | _ -> (mem, inst_num)
      in
      let output_mem =
        match instr with
        | Load (id, exp, _, loc) ->
            let locs = Sem.eval exp mem loc |> Dom.Val.get_all_locs in
            let v = Dom.Mem.find_heap_set locs mem in
            Dom.Mem.add_stack (Loc.of_var (Var.of_id id)) v mem
            |> Dom.Mem.load_alias id exp
        | Store (exp1, _, exp2, loc) ->
            let locs = Sem.eval exp1 mem loc |> Dom.Val.get_all_locs in
            Dom.Mem.update_mem locs (Sem.eval exp2 mem loc) mem
            |> Dom.Mem.store_alias exp1 exp2
        | Prune (exp, loc, _, _) -> Sem.prune exp loc mem
        | Call (ret, Const (Cfun callee_pname), params, loc, _) ->
            (match Summary.read_summary pdesc callee_pname with
             | Some summary ->
                 let callee = extras callee_pname in
                 let ret_val =
                   instantiate_ret tenv callee callee_pname params mem summary loc
                 in
                 (match ret with
                  | Some (id, _) ->
                      Dom.Mem.add_stack (Loc.of_var (Var.of_id id)) ret_val mem
                  | _ -> mem)
             | None ->
                 handle_unknown_call pname ret callee_pname params node mem loc)
        | Declare_locals (locals, _) ->
            (* array allocation in stack e.g., int arr[10] *)
            let (mem, inst_num) = List.fold ~f:try_decl_arr ~init:(mem, 1) locals in
            declare_symbolic_parameter pdesc tenv node inst_num mem
        | Call _
        | Remove_temps _
        | Abstract _
        | Nullify _ -> mem
      in
      print_debug_info instr mem output_mem;
      output_mem
end

module Analyzer = AbstractInterpreter.Make (ProcCfg.Normal) (TransferFunctions)

module Interprocedural = AbstractInterpreter.Interprocedural (Summary)
module CFG = Analyzer.TransferFunctions.CFG
module Sem = BufferOverrunSemantics.Make (CFG)

module Report =
struct
  type extras = Typ.Procname.t -> Procdesc.t option

  let add_condition
    : Typ.Procname.t -> CFG.node -> Exp.t -> Location.t -> Dom.Mem.astate
      -> Dom.ConditionSet.t -> Dom.ConditionSet.t
    = fun pname node exp loc mem cond_set ->
      let array_access =
        match exp with
        | Exp.Var _ ->
            let arr = Sem.eval exp mem loc |> Dom.Val.get_array_blk in
            Some (arr, Itv.zero, true)
        | Exp.Lindex (e1, e2)
        | Exp.BinOp (Binop.PlusA, e1, e2) ->
            let arr = Sem.eval e1 mem loc |> Dom.Val.get_array_blk in
            let idx = Sem.eval e2 mem loc |> Dom.Val.get_itv in
            Some (arr, idx, true)
        | Exp.BinOp (Binop.MinusA, e1, e2) ->
            let arr = Sem.eval e1 mem loc |> Dom.Val.get_array_blk in
            let idx = Sem.eval e2 mem loc |> Dom.Val.get_itv in
            Some (arr, idx, false)
        | _ -> None
      in
      match array_access with
      | Some (arr, idx, is_plus) ->
          let site = Sem.get_allocsite pname node 0 0 in
          let size = ArrayBlk.sizeof arr in
          let offset = ArrayBlk.offsetof arr in
          let idx = (if is_plus then Itv.plus else Itv.minus) offset idx in
          (if Config.bo_debug >= 2 then
             (L.err "@[<v 2>Add condition :@,";
              L.err "array: %a@," ArrayBlk.pp arr;
              L.err "  idx: %a@," Itv.pp idx;
              L.err "@]@."));
          if size <> Itv.bot && idx <> Itv.bot then
            Dom.ConditionSet.add_bo_safety pname loc site ~size ~idx cond_set
          else cond_set
      | None -> cond_set

  let instantiate_cond
    : Tenv.t -> Typ.Procname.t -> Procdesc.t option -> (Exp.t * Typ.t) list
      -> Dom.Mem.astate -> Summary.payload -> Location.t -> Dom.ConditionSet.t
    = fun tenv caller_pname callee_pdesc params caller_mem summary loc ->
      let callee_entry_mem = Dom.Summary.get_input summary in
      let callee_cond = Dom.Summary.get_cond_set summary in
      match callee_pdesc with
      | Some pdesc ->
          let subst_map =
            Sem.get_subst_map tenv pdesc params caller_mem callee_entry_mem loc
          in
          let pname = Procdesc.get_proc_name pdesc in
          Dom.ConditionSet.subst callee_cond subst_map caller_pname pname loc
      | _ -> callee_cond

  let print_debug_info : Sil.instr -> Dom.Mem.astate -> Dom.ConditionSet.t -> unit
    = fun instr pre cond_set ->
      if Config.bo_debug >= 2 then
        (L.err "@\n@\n================================@\n";
         L.err "@[<v 2>Pre-state : @,%a" Dom.Mem.pp pre;
         L.err "@]@\n@\n%a" (Sil.pp_instr Pp.text) instr;
         L.err "@[<v 2>@\n@\n%a" Dom.ConditionSet.pp cond_set;
         L.err "@]@\n";
         L.err "================================@\n@.")

  let collect_instr
    : extras ProcData.t -> CFG.node -> Dom.ConditionSet.t * Dom.Mem.astate
      -> Sil.instr -> Dom.ConditionSet.t * Dom.Mem.astate
    = fun ({ pdesc; tenv; extras } as pdata) node (cond_set, mem) instr ->
      let pname = Procdesc.get_proc_name pdesc in
      let cond_set =
        match instr with
        | Sil.Load (_, exp, _, loc)
        | Sil.Store (exp, _, _, loc) ->
            add_condition pname node exp loc mem cond_set
        | Sil.Call (_, Const (Cfun callee_pname), params, loc, _) ->
            (match Summary.read_summary pdesc callee_pname with
             | Some summary ->
                 let callee = extras callee_pname in
                 instantiate_cond tenv pname callee params mem summary loc
                 |> Dom.ConditionSet.rm_invalid
                 |> Dom.ConditionSet.join cond_set
             | _ -> cond_set)
        | _ -> cond_set
      in
      let mem = Analyzer.TransferFunctions.exec_instr mem pdata node instr in
      print_debug_info instr mem cond_set;
      (cond_set, mem)

  let collect_instrs
    : extras ProcData.t -> CFG.node -> Sil.instr list -> Dom.Mem.astate
      -> Dom.ConditionSet.t -> Dom.ConditionSet.t
    = fun pdata node instrs mem cond_set ->
      List.fold ~f:(collect_instr pdata node) ~init:(cond_set, mem) instrs
      |> fst

  let collect_node
    : extras ProcData.t -> Analyzer.invariant_map -> Dom.ConditionSet.t ->
      CFG.node -> Dom.ConditionSet.t
    = fun pdata inv_map cond_set node ->
      let instrs = CFG.instr_ids node |> List.map ~f:fst in
      match Analyzer.extract_pre (CFG.id node) inv_map with
      | Some mem -> collect_instrs pdata node instrs mem cond_set
      | _ -> cond_set

  let collect : extras ProcData.t -> Analyzer.invariant_map -> Dom.ConditionSet.t
    = fun ({ pdesc } as pdata) inv_map ->
      let add_node1 acc node = collect_node pdata inv_map acc node in
      Procdesc.fold_nodes add_node1 Dom.ConditionSet.empty pdesc

  let report_error : Procdesc.t -> Dom.ConditionSet.t -> unit
    = fun pdesc conds ->
      let pname = Procdesc.get_proc_name pdesc in
      let report1 cond =
        let alarm = Dom.Condition.check cond in
        let (caller_pname, loc) =
          match Dom.Condition.get_trace cond with
          | Dom.Condition.Inter (caller_pname, _, loc) -> (caller_pname, loc)
          | Dom.Condition.Intra pname -> (pname, Dom.Condition.get_location cond)
        in
        match alarm with
        | None -> ()
        | Some bucket when Typ.Procname.equal pname caller_pname ->
            let description = Dom.Condition.to_string cond in
            let error_desc = Localise.desc_buffer_overrun bucket description in
            let exn =
              Exceptions.Checkers (Localise.to_issue_id Localise.buffer_overrun, error_desc) in
            let trace = [Errlog.make_trace_element 0 loc description []] in
            Reporting.log_error pname ~loc ~ltr:trace exn
        | _ -> ()
      in
      Dom.ConditionSet.iter report1 conds
end

let compute_post
  : Analyzer.TransferFunctions.extras ProcData.t -> Summary.payload option
  = fun { pdesc; tenv; extras = get_pdesc } ->
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
    Report.report_error pdesc cond_set;
    match entry_mem, exit_mem with
    | Some entry_mem, Some exit_mem ->
        Some (entry_mem, exit_mem, cond_set)
    | _ -> None

let print_summary : Typ.Procname.t -> Dom.Summary.t -> unit
  = fun proc_name s ->
    L.err "@\n@[<v 2>Summary of %a :@,%a@]@."
      Typ.Procname.pp proc_name
      Dom.Summary.pp_summary s

let checker : Callbacks.proc_callback_args -> Specs.summary
  = fun ({ summary } as callback) ->
    let proc_name = Specs.get_proc_name summary in
    let make_extras _ = callback.get_proc_desc in
    let updated_summary : Specs.summary =
      Interprocedural.compute_and_store_post
        ~compute_post
        ~make_extras
        callback in
    let post =
      updated_summary.payload.buffer_overrun in
    begin
      match post with
      | Some s when Config.bo_debug >= 1 -> print_summary proc_name s
      | _ -> ()
    end;
    updated_summary
