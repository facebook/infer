(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open AbstractDomain.Types
module F = Format

module ConfigName = struct
  type t = {config_class: Pvar.t; config_name: Fieldname.t} [@@deriving compare, equal]

  (** For readability, this prints short name of config. *)
  let pp f {config_name} = F.fprintf f "%a" Fieldname.pp config_name
end

module ConfigTopLifted = struct
  type t = ConfigName.t top_lifted

  let pp = AbstractDomain.TopLiftedUtils.pp ~pp:ConfigName.pp

  let leq = AbstractDomain.TopLiftedUtils.leq ~leq:(fun ~lhs ~rhs -> ConfigName.equal lhs rhs)

  let join x y =
    match (x, y) with
    | Top, _ | _, Top ->
        Top
    | NonTop x', NonTop y' ->
        if ConfigName.equal x' y' then x else Top


  let widen ~prev ~next ~num_iters:_ = join prev next
end

module ConfigWithLocation = struct
  type t = ConfigName.t * Location.t [@@deriving compare]

  let pp f (config, location) = F.fprintf f "%a at %a" ConfigName.pp config Location.pp location
end

module Mem = AbstractDomain.Map (Ident) (ConfigTopLifted)

module MarkerSet = struct
  include AbstractDomain.FiniteSet (IntLit)

  let pp f x =
    if is_empty x then F.pp_print_string f "{ }"
    else (
      F.fprintf f "@[{ " ;
      let is_first = ref true in
      iter
        (fun marker ->
          if !is_first then is_first := false else F.fprintf f ",@ " ;
          IntLit.pp f marker )
        x ;
      F.fprintf f " }@]" )
end

module InvMarkerSet = AbstractDomain.InvertedSet (IntLit)

module Context = struct
  (** We use opposite orders in collecting the sets of started and ended markers. This is because we
      want to keep the analyzer sound in design. In program points where multiple control flows
      join, we want to know "the markers that have been started, at least, one of the control flow"
      (i.e. may-started) and "the markers that have been ended in all of the control flows" (i.e.
      must-ended). *)
  type t = {started_markers: MarkerSet.t; ended_markers: InvMarkerSet.t}

  let pp f {started_markers; ended_markers} =
    F.fprintf f "@[@[started markers: %a@]@\n@[ended markers: %a@]@]" MarkerSet.pp started_markers
      InvMarkerSet.pp ended_markers


  let leq ~lhs ~rhs =
    MarkerSet.leq ~lhs:lhs.started_markers ~rhs:rhs.started_markers
    && InvMarkerSet.leq ~lhs:lhs.ended_markers ~rhs:rhs.ended_markers


  let join x y =
    { started_markers= MarkerSet.join x.started_markers y.started_markers
    ; ended_markers= InvMarkerSet.join x.ended_markers y.ended_markers }


  let widen ~prev ~next ~num_iters =
    { started_markers=
        MarkerSet.widen ~prev:prev.started_markers ~next:next.started_markers ~num_iters
    ; ended_markers= InvMarkerSet.widen ~prev:prev.ended_markers ~next:next.ended_markers ~num_iters
    }


  let init = {started_markers= MarkerSet.bottom; ended_markers= InvMarkerSet.top}

  let call_marker_start marker {started_markers; ended_markers} =
    { started_markers= MarkerSet.add marker started_markers
    ; ended_markers= InvMarkerSet.remove marker ended_markers }


  let call_marker_end marker {started_markers; ended_markers} =
    { started_markers= MarkerSet.remove marker started_markers
    ; ended_markers= InvMarkerSet.add marker ended_markers }


  let instantiate_callee ~callee_context ~caller_context =
    let started_markers =
      MarkerSet.join callee_context.started_markers caller_context.started_markers
      |> InvMarkerSet.fold MarkerSet.remove callee_context.ended_markers
    in
    let ended_markers =
      InvMarkerSet.join caller_context.ended_markers callee_context.ended_markers
      |> MarkerSet.fold InvMarkerSet.remove callee_context.started_markers
    in
    {started_markers; ended_markers}
end

module ConfigChecks = struct
  include AbstractDomain.Map (ConfigWithLocation) (Context)

  let weak_add k v location m =
    update (k, location) (function None -> Some v | Some v' -> Some (Context.join v v')) m
end

module Summary = struct
  type t = {context: Context.t; config_checks: ConfigChecks.t}

  let pp f {context; config_checks} =
    F.fprintf f "@[@[%a@]@\n@[config checks:@ %a@]@]" Context.pp context ConfigChecks.pp
      config_checks
end

module Dom = struct
  type t = {mem: Mem.t; context: Context.t; config_checks: ConfigChecks.t}

  let pp f {mem; context; config_checks} =
    F.fprintf f "@[@[mem:@ %a@]@\n@[%a@]@\n@[config checks:@ %a@]@]" Mem.pp mem Context.pp context
      ConfigChecks.pp config_checks


  let init = {mem= Mem.bottom; context= Context.init; config_checks= ConfigChecks.bottom}

  let join x y =
    { mem= Mem.join x.mem y.mem
    ; context= Context.join x.context y.context
    ; config_checks= ConfigChecks.join x.config_checks y.config_checks }


  let widen ~prev ~next ~num_iters =
    { mem= Mem.widen ~prev:prev.mem ~next:next.mem ~num_iters
    ; context= Context.widen ~prev:prev.context ~next:next.context ~num_iters
    ; config_checks= ConfigChecks.widen ~prev:prev.config_checks ~next:next.config_checks ~num_iters
    }


  let leq ~lhs ~rhs =
    Mem.leq ~lhs:lhs.mem ~rhs:rhs.mem
    && Context.leq ~lhs:lhs.context ~rhs:rhs.context
    && ConfigChecks.leq ~lhs:lhs.config_checks ~rhs:rhs.config_checks


  let load_config id ~config_class ~config_name ({mem} as astate) =
    {astate with mem= Mem.add id (NonTop {ConfigName.config_class; config_name}) mem}


  let call_marker_start marker ({context} as astate) =
    {astate with context= Context.call_marker_start marker context}


  let call_marker_end marker ({context} as astate) =
    {astate with context= Context.call_marker_end marker context}


  let call_config_check id location ({mem; context; config_checks} as astate) =
    match Mem.find_opt id mem with
    | Some (NonTop config) ->
        {astate with config_checks= ConfigChecks.weak_add config context location config_checks}
    | _ ->
        astate


  let instantiate_callee
      ~callee_summary:{Summary.context= callee_context; config_checks= callee_config_checks}
      location ({context= caller_context; config_checks= caller_config_checks} as astate) =
    let context = Context.instantiate_callee ~callee_context ~caller_context in
    let config_checks =
      ConfigChecks.fold
        (fun (config, _) callee_context acc ->
          let context = Context.instantiate_callee ~callee_context ~caller_context in
          ConfigChecks.weak_add config context location acc )
        callee_config_checks caller_config_checks
    in
    {astate with context; config_checks}


  let to_summary {context; config_checks} = {Summary.context; config_checks}
end

type analysis_data = {tenv: Tenv.t; get_summary: Procname.t -> Summary.t option}

module TransferFunctions = struct
  module CFG = ProcCfg.NormalOneInstrPerNode
  module Domain = Dom

  type nonrec analysis_data = analysis_data

  let exec_instr astate {tenv; get_summary} _node instr =
    match (instr : Sil.instr) with
    | Load {id; e= Lfield (Lvar config_class, config_name, _)}
      when FbGKInteraction.is_config_class config_class ->
        Dom.load_config id ~config_class ~config_name astate
    | Call (_, Const (Cfun callee), _ :: (Const (Cint marker), _) :: _, _, _)
      when FbGKInteraction.is_marker_start tenv callee ->
        Dom.call_marker_start marker astate
    | Call (_, Const (Cfun callee), _ :: (Const (Cint marker), _) :: _, _, _)
      when FbGKInteraction.is_marker_end tenv callee ->
        Dom.call_marker_end marker astate
    | Call (_, Const (Cfun callee), _ :: (Var id, _) :: _, location, _)
      when FbGKInteraction.is_config_check tenv callee ->
        Dom.call_config_check id location astate
    | Call (_, Const (Cfun callee), _, location, _) ->
        Option.value_map (get_summary callee) ~default:astate ~f:(fun callee_summary ->
            Dom.instantiate_callee ~callee_summary location astate )
    | _ ->
        astate


  let pp_session_name node fmt =
    F.fprintf fmt "Config checks between markers %a" CFG.Node.pp_id (CFG.Node.id node)
end

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

let checker {InterproceduralAnalysis.proc_desc; tenv; analyze_dependency} =
  let open IOption.Let_syntax in
  let get_summary pname = analyze_dependency pname >>| snd in
  Analyzer.compute_post {tenv; get_summary} ~initial:Dom.init proc_desc >>| Dom.to_summary
