(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

(** backward analysis for computing set of maybe-live variables at each program point *)

module VarSet = AbstractDomain.FiniteSet (Var)

module Exn = struct
  include AbstractDomain.Map (Int) (VarSet)

  let union = union (fun _ v1 v2 -> Some (VarSet.union v1 v2))

  let diff =
    merge (fun _ v1 v2 ->
        match (v1, v2) with
        | None, _ | Some _, None ->
            v1
        | Some v1, Some v2 ->
            let r = VarSet.diff v1 v2 in
            Option.some_if (not (VarSet.is_bottom r)) r )
end

module Domain = struct
  type t = {normal: VarSet.t; exn: Exn.t}

  let pp f {normal; exn} = F.fprintf f "@[@[normal:%a@],@ @[exn:%a@]@]" VarSet.pp normal Exn.pp exn

  let leq ~lhs ~rhs =
    VarSet.leq ~lhs:lhs.normal ~rhs:rhs.normal && Exn.leq ~lhs:lhs.exn ~rhs:rhs.exn


  let join x y = {normal= VarSet.join x.normal y.normal; exn= Exn.join x.exn y.exn}

  let widen ~prev ~next ~num_iters =
    { normal= VarSet.widen ~prev:prev.normal ~next:next.normal ~num_iters
    ; exn= Exn.widen ~prev:prev.exn ~next:next.exn ~num_iters }


  let bottom = {normal= VarSet.bottom; exn= Exn.bottom}

  let is_bottom {normal; exn} = VarSet.is_bottom normal && Exn.is_bottom exn

  let update_normal ~f x = {x with normal= f x.normal}

  let add var = update_normal ~f:(VarSet.add var)

  let remove var = update_normal ~f:(VarSet.remove var)

  let map_normal ~f x = f x.normal

  let mem var = map_normal ~f:(VarSet.mem var)

  let fold f x init = map_normal ~f:(fun normal -> VarSet.fold f normal init) x

  let union x y = {normal= VarSet.union x.normal y.normal; exn= Exn.union x.exn y.exn}

  let diff x y = {normal= VarSet.diff x.normal y.normal; exn= Exn.diff x.exn y.exn}

  let catch_entry try_id x = {normal= VarSet.empty; exn= Exn.add try_id x.normal x.exn}

  let try_entry try_id x = {x with exn= Exn.remove try_id x.exn}

  let add_live_in_catch x =
    { x with
      normal= Exn.fold (fun _ live_in_catch acc -> VarSet.join acc live_in_catch) x.exn x.normal }
end

(** only kill pvars that are local; don't kill those that can escape *)
let is_always_in_scope proc_desc pvar =
  Pvar.is_return pvar || Pvar.is_global pvar || Procdesc.is_captured_pvar proc_desc pvar


let json_error ~option_name ~expected ~actual =
  L.die UserError "When parsing option %s: expected %s but got '%s'" option_name expected
    (Yojson.Basic.Util.to_string actual)


let string_list_of_json ~option_name ~init = function
  | `List json ->
      List.fold json
        ~f:(fun acc json ->
          match json with
          | `String s ->
              s :: acc
          | _ ->
              json_error ~option_name ~expected:"string" ~actual:json )
        ~init
  | json ->
      json_error ~option_name ~expected:"list of strings" ~actual:json


module type LivenessConfig = sig
  val is_dangerous_destructor : Procname.t -> bool
end

(** Use this config to get a reliable liveness pre-analysis that tells you which variables are live
    at which program point *)
module PreAnalysisMode : LivenessConfig = struct
  (** do not do any funky stuff *)
  let is_dangerous_destructor _proc_name = false
end

(** Use this config to get a dead store checker that can take some liberties wrt a strict liveness
    analysis *)
module CheckerMode = struct
  let dangerous_destructor_matcher =
    QualifiedCppName.Match.of_fuzzy_qual_names
      (string_list_of_json ~option_name:"liveness-dangerous-classes" ~init:[]
         Config.liveness_dangerous_classes )


  (** hardcoded list of wrappers, mostly because they are impossible to specify as config options *)
  let standard_wrappers_matcher =
    QualifiedCppName.Match.of_fuzzy_qual_names ["std::unique_ptr"; "std::shared_ptr"]


  let is_dangerous_class_name class_name =
    Typ.Name.unqualified_name class_name
    |> QualifiedCppName.Match.match_qualifiers dangerous_destructor_matcher


  let is_wrapper_of_dangerous_class_name class_name =
    Typ.Name.unqualified_name class_name
    |> QualifiedCppName.Match.match_qualifiers standard_wrappers_matcher
    &&
    match Typ.Name.get_template_spec_info class_name with
    | Some (Template {args= TType {desc= Tstruct name} :: _; _}) ->
        is_dangerous_class_name name
    | _ ->
        false


  let is_dangerous_proc_name (proc_name : Procname.t) =
    match proc_name with
    | ObjC_Cpp cpp_pname ->
        is_dangerous_class_name cpp_pname.class_name
        || is_wrapper_of_dangerous_class_name cpp_pname.class_name
    | _ ->
        false


  let is_destructor (proc_name : Procname.t) =
    match proc_name with
    | ObjC_Cpp cpp_pname ->
        Procname.ObjC_Cpp.is_destructor cpp_pname
    | _ ->
        false


  let is_dangerous_destructor (proc_name : Procname.t) =
    is_destructor proc_name && is_dangerous_proc_name proc_name
end

(** compilers 101-style backward transfer functions for liveness analysis. gen a variable when it is
    read, kill the variable when it is assigned *)
module TransferFunctions (LConfig : LivenessConfig) (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type analysis_data = Procdesc.t

  (** add all of the vars read in [exp] to the live set *)
  let exp_add_live exp astate =
    let astate' =
      Exp.free_vars exp
      |> Sequence.fold ~init:astate ~f:(fun astate_acc id -> Domain.add (Var.of_id id) astate_acc)
    in
    Exp.program_vars exp
    |> Sequence.fold ~init:astate' ~f:(fun astate_acc pvar ->
           Domain.add (Var.of_pvar pvar) astate_acc )


  let add_live_actuals actuals live_acc =
    let actuals = List.map actuals ~f:(fun (e, _) -> Exp.ignore_cast e) in
    List.fold actuals ~f:(fun acc_ exp -> exp_add_live exp acc_) ~init:live_acc


  let exec_instr astate proc_desc _ _ = function
    | Sil.Load {id= lhs_id} when Ident.is_none lhs_id ->
        (* dummy deref inserted by frontend--don't count as a read *)
        astate
    | Sil.Load {id= lhs_id; e= rhs_exp} ->
        Domain.remove (Var.of_id lhs_id) astate |> exp_add_live rhs_exp
    | Sil.Store {e1= Lvar lhs_pvar; e2= rhs_exp} ->
        let astate' =
          if is_always_in_scope proc_desc lhs_pvar then astate (* never kill globals *)
          else Domain.remove (Var.of_pvar lhs_pvar) astate
        in
        exp_add_live rhs_exp astate'
    | Sil.Store {e1= lhs_exp; e2= rhs_exp} ->
        exp_add_live lhs_exp astate |> exp_add_live rhs_exp
    | Sil.Prune (exp, _, _, _) ->
        exp_add_live exp astate
    | Sil.Call ((ret_id, _), Const (Cfun callee_pname), _, _, _)
      when LConfig.is_dangerous_destructor callee_pname ->
        Logging.d_printfln_escaped "Dangerous destructor %a, ignoring reads@\n" Procname.pp
          callee_pname ;
        Domain.remove (Var.of_id ret_id) astate
    | Sil.Call ((ret_id, _), call_exp, actuals, _, {CallFlags.cf_assign_last_arg}) ->
        let actuals_to_read, astate =
          if cf_assign_last_arg then
            match IList.split_last_rev actuals with
            | Some ((Exp.Lvar pvar, _), actuals') when not (is_always_in_scope proc_desc pvar) ->
                (actuals', Domain.remove (Var.of_pvar pvar) astate)
            | _ ->
                (actuals, astate)
          else (actuals, astate)
        in
        Domain.remove (Var.of_id ret_id) astate
        |> exp_add_live call_exp |> add_live_actuals actuals_to_read
        |> (* assume that all function calls can throw for now *)
        Domain.add_live_in_catch
    | Sil.Metadata (CatchEntry {try_id}) ->
        Domain.catch_entry try_id astate
    | Sil.Metadata (TryEntry {try_id}) ->
        Domain.try_entry try_id astate
    | Sil.Metadata _ ->
        astate


  let pp_session_name node fmt = F.fprintf fmt "liveness %a" CFG.Node.pp_id (CFG.Node.id node)
end

module CFG = ProcCfg.OneInstrPerNode (ProcCfg.Backward (ProcCfg.Exceptional))
module CheckerAnalyzer = AbstractInterpreter.MakeRPO (TransferFunctions (CheckerMode) (CFG))
module PreAnalysisTransferFunctions = TransferFunctions (PreAnalysisMode)

(* It's fine to have a dead store on a type that uses the "scope guard" pattern. These types
   are only read in their destructors, and this is expected/ok.
   (e.g., https://github.com/facebook/folly/blob/master/folly/ScopeGuard.h). *)
let matcher_scope_guard =
  let default_scope_guards = ["CKComponentKey"; "CKComponentScope"] in
  string_list_of_json ~option_name:"cxx-scope_guards" ~init:default_scope_guards
    Config.cxx_scope_guards
  |> QualifiedCppName.Match.of_fuzzy_qual_names


module PassedByRefTransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = VarSet

  type analysis_data = unit

  let add_if_lvar expr astate =
    match Exp.ignore_cast expr with
    | Exp.Lvar pvar ->
        (* passed or captured by reference, add *)
        Domain.add (Var.of_pvar pvar) astate
    | _ ->
        (* passed or captured by value or init-capture, skip *)
        astate


  let proc_name_of_expr expr =
    match (expr : Exp.t) with Const (Cfun proc_name) -> Some proc_name | _ -> None


  let is_dangerous expr =
    (* ignore all captures from "dangerous" classes *)
    proc_name_of_expr expr |> Option.exists ~f:CheckerMode.is_dangerous_proc_name


  let exec_instr astate () _ _ (instr : Sil.instr) =
    let astate =
      match instr with
      | Call (_ret, f, actuals, _loc, _flags) when not (is_dangerous f) ->
          let actuals =
            if Option.exists (proc_name_of_expr f) ~f:Procname.is_constructor then
              (* Skip "this" in constructors, assuming constructors are less likely to have global
                 side effects that store "this" in the global state. We could also skip "this" in
                 all (non-static) methods but this becomes less clear: constructing an object then
                 calling methods on it can have side-effects even if the object is used for nothing
                 else. *)
              List.tl actuals |> Option.value ~default:[]
            else actuals
          in
          List.fold actuals ~init:astate ~f:(fun astate (actual, _typ) -> add_if_lvar actual astate)
      | _ ->
          astate
    in
    List.fold (Sil.exps_of_instr instr) ~init:astate ~f:(fun astate exp ->
        Exp.fold_captured exp ~f:(fun astate exp -> add_if_lvar exp astate) astate )


  let pp_session_name _node fmt = F.pp_print_string fmt "passed by reference"
end

module PassedByRefAnalyzer =
  AbstractInterpreter.MakeRPO (PassedByRefTransferFunctions (ProcCfg.Exceptional))

let get_passed_by_ref_invariant_map proc_desc =
  let cfg = ProcCfg.Exceptional.from_pdesc proc_desc in
  PassedByRefAnalyzer.exec_cfg cfg () ~initial:VarSet.empty


module IntLitSet = Caml.Set.Make (IntLit)

let ignored_constants =
  let int_lit_constants =
    List.map
      ~f:(fun el ->
        try IntLit.of_string el
        with Invalid_argument _ ->
          L.die UserError
            "Ill-formed option  '%s' for --liveness-ignored-constant: an integer was expected" el )
      Config.liveness_ignored_constant
  in
  IntLitSet.of_list int_lit_constants


let checker {IntraproceduralAnalysis.proc_desc; err_log} =
  let passed_by_ref_invariant_map = get_passed_by_ref_invariant_map proc_desc in
  let cfg = CFG.from_pdesc proc_desc in
  let invariant_map = CheckerAnalyzer.exec_cfg cfg proc_desc ~initial:Domain.bottom in
  (* we don't want to report in harmless cases like int i = 0; if (...) { i = ... } else { i = ... }
     that create an intentional dead store as an attempt to imitate default value semantics.
     use dead stores to a "sentinel" value as a heuristic for ignoring this case *)
  let rec is_sentinel_exp = function
    | Exp.Cast (_, e) ->
        is_sentinel_exp e
    | Exp.Const (Cint i) ->
        IntLitSet.mem i ignored_constants
    | Exp.Const (Cfloat f) -> (
      match Z.of_float f with
      | z ->
          IntLitSet.mem (IntLit.of_big_int z) ignored_constants
      | exception Z.Overflow ->
          false )
    | _ ->
        false
  in
  let rec is_scope_guard = function
    | {Typ.desc= Tstruct name} ->
        QualifiedCppName.Match.match_qualifiers matcher_scope_guard (Typ.Name.qual_name name)
    | {Typ.desc= Tptr (typ, _)} ->
        is_scope_guard typ
    | _ ->
        false
  in
  let locals = Procdesc.get_locals proc_desc in
  let is_constexpr_or_unused pvar =
    List.find locals ~f:(fun local_data ->
        Mangled.equal (Pvar.get_name pvar) local_data.ProcAttributes.name )
    |> Option.exists ~f:(fun local ->
           local.ProcAttributes.is_constexpr || local.ProcAttributes.is_declared_unused )
  in
  let should_report pvar typ live_vars passed_by_ref_vars =
    not
      ( Pvar.is_frontend_tmp pvar || Pvar.is_return pvar || Pvar.is_global pvar
      || is_constexpr_or_unused pvar
      || VarSet.mem (Var.of_pvar pvar) passed_by_ref_vars
      || Domain.mem (Var.of_pvar pvar) live_vars
      || Procdesc.is_captured_pvar proc_desc pvar
      || is_scope_guard typ
      || Procdesc.has_modify_in_block_attr proc_desc pvar )
  in
  let log_report pvar typ loc =
    let message =
      F.asprintf "The value written to `%a` (type `%a`) is never used" (Pvar.pp Pp.text) pvar
        (Typ.pp_full Pp.text) typ
    in
    let ltr = [Errlog.make_trace_element 0 loc "Write of unused value" []] in
    Reporting.log_issue proc_desc err_log ~loc ~ltr Liveness IssueType.dead_store message
  in
  let report_dead_store live_vars passed_by_ref_vars = function
    | Sil.Store {e1= Lvar pvar; typ; e2= rhs_exp; loc}
      when should_report pvar typ live_vars passed_by_ref_vars && not (is_sentinel_exp rhs_exp) ->
        log_report pvar typ loc
    | Sil.Call (_, e_fun, (arg, typ) :: _, loc, _) -> (
      match (Exp.ignore_cast e_fun, Exp.ignore_cast arg) with
      | Exp.Const (Cfun (Procname.ObjC_Cpp _ as pname)), Exp.Lvar pvar
        when Procname.is_constructor pname && should_report pvar typ live_vars passed_by_ref_vars ->
          log_report pvar typ loc
      | _, _ ->
          () )
    | _ ->
        ()
  in
  let report_on_node node =
    let passed_by_ref_vars =
      match
        PassedByRefAnalyzer.extract_post
          (ProcCfg.Exceptional.Node.id (CFG.Node.underlying_node node))
          passed_by_ref_invariant_map
      with
      | Some post ->
          post
      | None ->
          VarSet.empty
    in
    let node_id = CFG.Node.id node in
    Instrs.iter (CFG.instrs node) ~f:(fun instr ->
        match CheckerAnalyzer.extract_pre node_id invariant_map with
        | Some live_vars ->
            report_dead_store live_vars passed_by_ref_vars instr
        | None ->
            () )
  in
  Container.iter cfg ~fold:CFG.fold_nodes ~f:report_on_node
