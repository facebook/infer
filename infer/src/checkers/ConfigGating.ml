(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Config Gating Analysis: determines which config flags gate the execution of each call site.

    The abstract state tracks, for each config, which values it MAY have if execution reaches this
    point. A config absent from the map may have any value (unconstrained). A call site is "gated"
    if some config is constrained to a strict subset of possible values. *)

(** Identifies a config flag. *)
module ConfigName = struct
  type t = string [@@deriving compare, equal]

  let pp = F.pp_print_string
end

(** Which value the config may have on this path. *)
module BranchVal = struct
  type t = True | False [@@deriving compare, equal]

  let pp fmt = function
    | True ->
        F.pp_print_string fmt "true"
    | False ->
        F.pp_print_string fmt "false"


  let negate = function True -> False | False -> True
end

(** For each config, the set of values it may have if this point is reached. Absent config =
    unconstrained (may be any value). Join = union of possible value sets per config; if a config
    becomes unconstrained (both True and False are possible), it is removed. Empty map = no config
    is constrained = code always executes. *)
module ConfigGuards = struct
  module M = Stdlib.Map.Make (ConfigName)

  type t = BranchVal.t M.t

  let pp fmt m =
    if M.is_empty m then F.pp_print_string fmt "{}"
    else
      let pp_binding fmt (k, v) = F.fprintf fmt "%a -> %a" ConfigName.pp k BranchVal.pp v in
      F.fprintf fmt "{ %a }"
        (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ", ") pp_binding)
        (M.bindings m)


  let empty = M.empty

  let is_empty = M.is_empty

  let add = M.add

  let leq ~lhs ~rhs =
    (* a ≤ b iff b does not constrain config flag values more than a does. For example,
       {foo -> true} ≤ {} because the rhs does not constrain foo while the lhs does. Equivalently,
       every constraint in b must also be present (with the same value) in a. *)
    M.for_all
      (fun config rhs_val -> Option.exists (M.find_opt config lhs) ~f:(BranchVal.equal rhs_val))
      rhs


  let join a b =
    (* Union of possible value sets: for each config, compute {values in a} ∪ {values in b}.
       A config absent from one side has the full value set {True, False} (unconstrained), so the
       union is the full set — remove it. For configs present in both, if the values agree the
       config stays constrained; if they differ, the union is {True, False} — remove it. *)
    M.merge
      (fun _config v1 v2 ->
        match (v1, v2) with Some x, Some y when BranchVal.equal x y -> Some x | _ -> None )
      a b


  let widen ~prev ~next ~num_iters:_ = join prev next
end

(** Tracks which variables hold config values. *)
module ConfigFlat = AbstractDomain.Flat (ConfigName)

module Mem = AbstractDomain.Map (Var) (ConfigFlat)

(** Full abstract domain: config guards + variable-to-config memory. *)
module Domain = struct
  type t = {guards: ConfigGuards.t; mem: Mem.t}

  let pp fmt {guards; mem} =
    F.fprintf fmt "@[guards: %a@;mem: %a@]" ConfigGuards.pp guards Mem.pp mem


  let leq ~lhs ~rhs =
    ConfigGuards.leq ~lhs:lhs.guards ~rhs:rhs.guards && Mem.leq ~lhs:lhs.mem ~rhs:rhs.mem


  let join a b = {guards= ConfigGuards.join a.guards b.guards; mem= Mem.join a.mem b.mem}

  let widen ~prev ~next ~num_iters =
    { guards= ConfigGuards.widen ~prev:prev.guards ~next:next.guards ~num_iters
    ; mem= Mem.widen ~prev:prev.mem ~next:next.mem ~num_iters }
end

(** Check whether a callee matches any of the configured config-gating method patterns. The callee
    is matched as "ClassName.methodName" against each regex pattern. *)
let is_config_method procname =
  let qualified =
    match Procname.get_class_name procname with
    | Some class_name ->
        class_name ^ "." ^ Procname.get_method procname
    | None ->
        Procname.get_method procname
  in
  List.exists Config.config_gating_method_patterns ~f:(fun re ->
      match Str.search_forward re qualified 0 with _ -> true | exception Stdlib.Not_found -> false )


(** Extract a config name from the call arguments. Tries string constant first (static method
    pattern), then int constant at arg index 1 (instance method pattern with receiver at index 0).
*)
let get_config_name _callee args =
  match args with
  | (Exp.Const (Cstr config_name), _) :: _ ->
      config_name
  | _receiver :: (Exp.Const (Cint field_code), _) :: _ ->
      F.asprintf "config:%a" IntLit.pp field_code
  | _ ->
      "unknown_config"


(** Extract config info from a pruned expression: returns (config_name, branch_value) indicating
    which value the config may have on this path. *)
let rec get_config_from_exp mem exp =
  match (exp : Exp.t) with
  | Var id ->
      let var = Var.of_id id in
      Mem.find_opt var mem |> Option.bind ~f:ConfigFlat.get
      |> Option.map ~f:(fun cfg -> (cfg, BranchVal.True))
  | UnOp (LNot, e, _) ->
      get_config_from_exp mem e
      |> Option.map ~f:(fun (cfg, branch) -> (cfg, BranchVal.negate branch))
  | (BinOp (Eq, Var id, Const (Cint i)) | BinOp (Eq, Const (Cint i), Var id)) when IntLit.iszero i
    ->
      (* id == 0 means config is false on this path *)
      let var = Var.of_id id in
      Mem.find_opt var mem |> Option.bind ~f:ConfigFlat.get
      |> Option.map ~f:(fun cfg -> (cfg, BranchVal.False))
  | (BinOp (Ne, Var id, Const (Cint i)) | BinOp (Ne, Const (Cint i), Var id)) when IntLit.iszero i
    ->
      (* id != 0 means config is true on this path *)
      let var = Var.of_id id in
      Mem.find_opt var mem |> Option.bind ~f:ConfigFlat.get
      |> Option.map ~f:(fun cfg -> (cfg, BranchVal.True))
  | _ ->
      None


module TransferFunctions = struct
  module CFG = ProcCfg.OneInstrPerNode (ProcCfg.Normal)
  module Domain = Domain

  type analysis_data = unit

  let exec_instr (astate : Domain.t) () _node _idx (instr : Sil.instr) =
    match instr with
    | Call ((ret_id, _), Const (Cfun callee), args, _loc, _flags) when is_config_method callee ->
        let config_name = get_config_name callee args in
        let mem = Mem.add (Var.of_id ret_id) (ConfigFlat.v config_name) astate.mem in
        {astate with mem}
    | Prune (exp, _loc, _is_then_branch, _if_kind) -> (
      match get_config_from_exp astate.mem exp with
      | Some (config_name, branch) ->
          {astate with guards= ConfigGuards.add config_name branch astate.guards}
      | None ->
          astate )
    | Load {id; e= Lvar pvar} -> (
        let var_pv = Var.of_pvar pvar in
        match Mem.find_opt var_pv astate.mem with
        | Some config_val ->
            {astate with mem= Mem.add (Var.of_id id) config_val astate.mem}
        | None ->
            astate )
    | Store {e1= Lvar pvar; e2= Exp.Var id} -> (
        let var_id = Var.of_id id in
        match Mem.find_opt var_id astate.mem with
        | Some config_val ->
            {astate with mem= Mem.add (Var.of_pvar pvar) config_val astate.mem}
        | None ->
            astate )
    | _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "config gating"
end

module CFG = TransferFunctions.CFG
module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions)

let checker {IntraproceduralAnalysis.proc_desc; err_log} =
  let cfg = CFG.from_pdesc proc_desc in
  let initial = {Domain.guards= ConfigGuards.empty; mem= Mem.bottom} in
  let invariant_map = Analyzer.exec_cfg cfg () ~initial in
  Container.iter cfg ~fold:CFG.fold_nodes ~f:(fun node ->
      let node_id = CFG.Node.id node in
      match Analyzer.extract_pre node_id invariant_map with
      | Some ({guards; _} : Domain.t) when not (ConfigGuards.is_empty guards) ->
          Instrs.iter (CFG.instrs node) ~f:(fun instr ->
              match instr with
              | Sil.Call (_, Exp.Const (Cfun callee), _, loc, _) when not (is_config_method callee)
                ->
                  let message =
                    F.asprintf "Call to %a is gated by: %a" Procname.pp callee ConfigGuards.pp
                      guards
                  in
                  let ltr = [Errlog.make_trace_element 0 loc message []] in
                  Reporting.log_issue proc_desc err_log ~loc ~ltr ConfigGating
                    IssueType.config_gated message
              | _ ->
                  () )
      | _ ->
          () )
