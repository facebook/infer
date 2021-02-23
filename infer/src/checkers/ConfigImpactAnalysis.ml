(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module ConfigName = FbGKInteraction.ConfigName

module Branch = struct
  type t = True | False | Top

  let pp f = function
    | True ->
        F.pp_print_string f "true branch"
    | False ->
        F.pp_print_string f "false branch"
    | Top ->
        AbstractDomain.TopLiftedUtils.pp_top f


  let leq ~lhs ~rhs =
    match (lhs, rhs) with True, True | False, False | _, Top -> true | _, _ -> false


  let join x y = match (x, y) with True, True -> True | False, False -> False | _, _ -> Top

  let widen ~prev ~next ~num_iters:_ = join prev next

  let top = Top

  let is_top = function Top -> true | True | False -> false

  let neg = function True -> False | False -> True | Top -> Top
end

module ConfigChecks = AbstractDomain.SafeInvertedMap (ConfigName) (Branch)

module UncheckedCallee = struct
  type t =
    | Direct of {callee: Procname.t; location: Location.t}
    | Indirect of {callee: Procname.t; location: Location.t}
  [@@deriving compare]

  let pp f = function
    | Direct {callee; location} ->
        F.fprintf f "%a is called at %a" Procname.pp callee Location.pp location
    | Indirect {callee; location} ->
        F.fprintf f "%a is indirectly called from %a" Procname.pp callee Location.pp location


  let get_location = function Direct {location} | Indirect {location} -> location

  let replace_location location = function
    | Direct {callee} | Indirect {callee} ->
        Indirect {callee; location}


  let report {InterproceduralAnalysis.proc_desc; err_log} x =
    let desc = F.asprintf "%a without config check" pp x in
    let trace =
      (* TODO *)
      []
    in
    Reporting.log_issue proc_desc err_log ~loc:(get_location x) ~ltr:trace ConfigImpactAnalysis
      IssueType.config_impact_analysis desc
end

module UncheckedCallees = struct
  include AbstractDomain.FiniteSet (UncheckedCallee)

  let report analysis_data unchecked_callees =
    iter (UncheckedCallee.report analysis_data) unchecked_callees


  let replace_location location x = map (UncheckedCallee.replace_location location) x
end

module Loc = struct
  type t = Ident of Ident.t | Pvar of Pvar.t [@@deriving compare]

  let pp f = function Ident id -> Ident.pp f id | Pvar pvar -> Pvar.pp Pp.text f pvar

  let of_id id = Ident id

  let of_pvar pvar = Pvar pvar
end

module ConfigLifted = AbstractDomain.Flat (ConfigName)

module Val = struct
  type t = {config: ConfigLifted.t}

  let pp f {config} = F.fprintf f "@[config:@,%a@]" ConfigLifted.pp config

  let leq ~lhs ~rhs = ConfigLifted.leq ~lhs:lhs.config ~rhs:rhs.config

  let join x y = {config= ConfigLifted.join x.config y.config}

  let widen ~prev ~next ~num_iters =
    {config= ConfigLifted.widen ~prev:prev.config ~next:next.config ~num_iters}


  let bottom = {config= ConfigLifted.bottom}

  let of_config config = {config= ConfigLifted.v config}

  let get_config_opt {config} = ConfigLifted.get config
end

module Mem = struct
  include AbstractDomain.Map (Loc) (Val)

  let lookup loc mem = find_opt loc mem |> Option.value ~default:Val.bottom
end

module Summary = struct
  type t = {unchecked_callees: UncheckedCallees.t}

  let pp f {unchecked_callees} =
    F.fprintf f "@[unchecked callees:@,%a@]" UncheckedCallees.pp unchecked_callees
end

module Dom = struct
  type t = {config_checks: ConfigChecks.t; unchecked_callees: UncheckedCallees.t; mem: Mem.t}

  let pp f {config_checks; unchecked_callees; mem} =
    F.fprintf f "@[@[config checks:@,%a@]@ @[unchecked callees:@,%a@]@ @[mem:%,%a@]@]"
      ConfigChecks.pp config_checks UncheckedCallees.pp unchecked_callees Mem.pp mem


  let leq ~lhs ~rhs =
    ConfigChecks.leq ~lhs:lhs.config_checks ~rhs:rhs.config_checks
    && UncheckedCallees.leq ~lhs:lhs.unchecked_callees ~rhs:rhs.unchecked_callees
    && Mem.leq ~lhs:lhs.mem ~rhs:rhs.mem


  let join x y =
    { config_checks= ConfigChecks.join x.config_checks y.config_checks
    ; unchecked_callees= UncheckedCallees.join x.unchecked_callees y.unchecked_callees
    ; mem= Mem.join x.mem y.mem }


  let widen ~prev ~next ~num_iters =
    { config_checks= ConfigChecks.widen ~prev:prev.config_checks ~next:next.config_checks ~num_iters
    ; unchecked_callees=
        UncheckedCallees.widen ~prev:prev.unchecked_callees ~next:next.unchecked_callees ~num_iters
    ; mem= Mem.widen ~prev:prev.mem ~next:next.mem ~num_iters }


  let to_summary {unchecked_callees} = {Summary.unchecked_callees}

  let init =
    {config_checks= ConfigChecks.top; unchecked_callees= UncheckedCallees.bottom; mem= Mem.bottom}


  let add_mem loc v ({mem} as astate) = {astate with mem= Mem.add loc v mem}

  let copy_mem ~tgt ~src ({mem} as astate) = add_mem tgt (Mem.lookup src mem) astate

  let call_config_check ret config astate = add_mem (Loc.of_id ret) (Val.of_config config) astate

  let load_config id pvar astate = copy_mem ~tgt:(Loc.of_id id) ~src:(Loc.of_pvar pvar) astate

  let store_config pvar id astate = copy_mem ~tgt:(Loc.of_pvar pvar) ~src:(Loc.of_id id) astate

  let boolean_value id_tgt id_src astate =
    copy_mem ~tgt:(Loc.of_id id_tgt) ~src:(Loc.of_id id_src) astate


  let neg_branch res = Option.map ~f:(fun (config, branch) -> (config, Branch.neg branch)) res

  let rec get_config_check_prune e mem =
    match (e : Exp.t) with
    | Var id ->
        Mem.lookup (Loc.of_id id) mem
        |> Val.get_config_opt
        |> Option.map ~f:(fun config -> (config, Branch.True))
    | UnOp (LNot, e, _) ->
        get_config_check_prune e mem |> neg_branch
    | BinOp ((Eq | Ne), Const _, Const _) ->
        None
    | (BinOp (Eq, e, (Const _ as const)) | BinOp (Eq, (Const _ as const), e)) when Exp.is_zero const
      ->
        get_config_check_prune e mem |> neg_branch
    | (BinOp (Ne, e, (Const _ as const)) | BinOp (Ne, (Const _ as const), e)) when Exp.is_zero const
      ->
        get_config_check_prune e mem
    | _ ->
        None


  let prune e ({config_checks; mem} as astate) =
    get_config_check_prune e mem
    |> Option.value_map ~default:astate ~f:(fun (config, branch) ->
           {astate with config_checks= ConfigChecks.add config branch config_checks} )


  let call analyze_dependency callee location ({config_checks; unchecked_callees} as astate) =
    if ConfigChecks.is_top config_checks then
      let unchecked_callees =
        match analyze_dependency callee with
        | Some (_, {Summary.unchecked_callees= callee_summary})
          when not (UncheckedCallees.is_bottom callee_summary) ->
            (* If callee's summary is non-bottom, use it. *)
            UncheckedCallees.replace_location location callee_summary
            |> UncheckedCallees.join unchecked_callees
        | _ ->
            (* Otherwise, add callee's name. *)
            UncheckedCallees.add (UncheckedCallee.Direct {callee; location}) unchecked_callees
      in
      {astate with unchecked_callees}
    else astate
end

module TransferFunctions = struct
  module CFG = ProcCfg.NormalOneInstrPerNode
  module Domain = Dom

  type analysis_data = Summary.t InterproceduralAnalysis.t

  let is_java_boolean_value_method pname =
    Procname.get_class_name pname |> Option.exists ~f:(String.equal "java.lang.Boolean")
    && Procname.get_method pname |> String.equal "booleanValue"


  let exec_instr astate {InterproceduralAnalysis.tenv; analyze_dependency} _node instr =
    match (instr : Sil.instr) with
    | Load {id; e= Lvar pvar} ->
        Dom.load_config id pvar astate
    | Store {e1= Lvar pvar; e2= Var id} ->
        Dom.store_config pvar id astate
    | Call ((ret, _), Const (Cfun callee), [(Var id, _)], _, _)
      when is_java_boolean_value_method callee ->
        Dom.boolean_value ret id astate
    | Call ((ret, _), Const (Cfun callee), args, location, _) -> (
      match FbGKInteraction.get_config_check tenv callee args with
      | Some (`Config config) ->
          Dom.call_config_check ret config astate
      | Some (`Exp _) ->
          astate
      | None ->
          (* normal function calls *)
          Dom.call analyze_dependency callee location astate )
    | Prune (e, _, _, _) ->
        Dom.prune e astate
    | _ ->
        astate


  let pp_session_name node fmt =
    F.fprintf fmt "Config impact function calls %a" CFG.Node.pp_id (CFG.Node.id node)
end

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) =
  Option.map (Analyzer.compute_post analysis_data ~initial:Dom.init proc_desc)
    ~f:(fun ({Dom.unchecked_callees} as astate) ->
      UncheckedCallees.report analysis_data unchecked_callees ;
      Dom.to_summary astate )
