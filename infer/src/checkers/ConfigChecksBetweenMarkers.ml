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

module Trace = struct
  type elem = MarkerStart of IntLit.t | ConfigCheck of ConfigName.t

  type t =
    | Empty
    | Elem of {kind: elem; location: Location.t; from: t}
    | Call of {callee: Procname.t; location: Location.t; from: t; callee_trace: t}

  let pp_elem f = function
    | MarkerStart i ->
        F.fprintf f "MarkerStart(%a)" IntLit.pp i
    | ConfigCheck config ->
        F.fprintf f "ConfigCheck(%a)" ConfigName.pp config


  let rec pp f = function
    | Empty ->
        F.pp_print_string f "Empty"
    | Elem {kind; from} ->
        F.fprintf f "Elem(%a,%a)" pp_elem kind pp from
    | Call {callee; from; callee_trace} ->
        F.fprintf f "Call(%a,%a,%a)" pp from Procname.pp callee pp callee_trace


  (** [Trace] is additional information, thus which should not affect analysis operation *)
  let leq ~lhs:_ ~rhs:_ = true

  let join x _y = x

  let widen ~prev ~next ~num_iters:_ = join prev next

  let marker_start marker = MarkerStart marker

  let config_check config = ConfigCheck config

  let add_elem kind location from = Elem {kind; location; from}

  let add_call callee location ~from ~callee_trace = Call {callee; location; from; callee_trace}

  let singleton kind location = add_elem kind location Empty
end

module TraceWithNothing = struct
  type t = {trace: Trace.t}

  let pp f {trace} = Trace.pp f trace

  let leq ~lhs ~rhs = Trace.leq ~lhs:lhs.trace ~rhs:rhs.trace

  let join x y = {trace= Trace.join x.trace y.trace}

  let widen ~prev ~next ~num_iters =
    {trace= Trace.widen ~prev:prev.trace ~next:next.trace ~num_iters}
end

module MarkerSet = struct
  include AbstractDomain.Map (IntLit) (TraceWithNothing)

  let pp f x =
    if is_empty x then F.pp_print_string f "{ }"
    else (
      F.fprintf f "@[{ " ;
      let is_first = ref true in
      iter
        (fun marker _trace ->
          if !is_first then is_first := false else F.fprintf f ",@ " ;
          IntLit.pp f marker )
        x ;
      F.fprintf f " }@]" )


  let add_trace new_trace location x =
    map (fun {trace} -> {trace= Trace.add_elem new_trace location trace}) x


  let join_on_call callee_pname ?config_check_trace location ~callee ~caller =
    merge
      (fun _marker callee_trace caller_trace ->
        let add_call {TraceWithNothing.trace= callee_trace} =
          let from =
            Option.value_map caller_trace ~default:Trace.Empty ~f:(fun {TraceWithNothing.trace} ->
                trace )
          in
          let trace = Trace.add_call callee_pname location ~from ~callee_trace in
          Some {TraceWithNothing.trace}
        in
        match (callee_trace, config_check_trace) with
        | None, None ->
            caller_trace
        | None, Some config_check_trace ->
            add_call {TraceWithNothing.trace= config_check_trace}
        | Some callee_trace, _ ->
            add_call callee_trace )
      callee caller
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

  let call_marker_start marker location {started_markers; ended_markers} =
    let trace = Trace.singleton (Trace.marker_start marker) location in
    let trace_dom = {TraceWithNothing.trace} in
    { started_markers= MarkerSet.add marker trace_dom started_markers
    ; ended_markers= InvMarkerSet.remove marker ended_markers }


  let call_marker_end marker {started_markers; ended_markers} =
    { started_markers= MarkerSet.remove marker started_markers
    ; ended_markers= InvMarkerSet.add marker ended_markers }


  let call_config_check new_trace location ({started_markers} as context) =
    {context with started_markers= MarkerSet.add_trace new_trace location started_markers}


  let instantiate_callee ~callee_pname ?config_check_trace location ~callee_context ~caller_context
      =
    let started_markers =
      MarkerSet.join_on_call callee_pname ?config_check_trace location
        ~callee:callee_context.started_markers ~caller:caller_context.started_markers
      |> InvMarkerSet.fold MarkerSet.remove callee_context.ended_markers
    in
    let ended_markers =
      InvMarkerSet.join caller_context.ended_markers callee_context.ended_markers
      |> MarkerSet.fold
           (fun marker _trace acc -> InvMarkerSet.remove marker acc)
           callee_context.started_markers
    in
    {started_markers; ended_markers}
end

module ContextWithTrace = struct
  type t = {context: Context.t; trace: Trace.t}

  let pp f {context; trace= _} = Context.pp f context

  let leq ~lhs ~rhs = Context.leq ~lhs:lhs.context ~rhs:rhs.context

  let join x y = {context= Context.join x.context y.context; trace= Trace.join x.trace y.trace}

  let widen ~prev ~next ~num_iters =
    { context= Context.widen ~prev:prev.context ~next:next.context ~num_iters
    ; trace= Trace.widen ~prev:prev.trace ~next:next.trace ~num_iters }
end

module ConfigChecks = struct
  include AbstractDomain.Map (ConfigWithLocation) (ContextWithTrace)

  let weak_add k v location m =
    update (k, location)
      (function None -> Some v | Some v' -> Some (ContextWithTrace.join v v'))
      m
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


  let call_marker_start marker location ({context} as astate) =
    {astate with context= Context.call_marker_start marker location context}


  let call_marker_end marker ({context} as astate) =
    {astate with context= Context.call_marker_end marker context}


  let call_config_check id location ({mem; context; config_checks} as astate) =
    match Mem.find_opt id mem with
    | Some (NonTop config) ->
        let trace_elem = Trace.config_check config in
        let context = Context.call_config_check trace_elem location context in
        let context_with_trace =
          {ContextWithTrace.context; trace= Trace.singleton trace_elem location}
        in
        { astate with
          config_checks= ConfigChecks.weak_add config context_with_trace location config_checks }
    | _ ->
        astate


  let instantiate_callee ~callee
      ~callee_summary:{Summary.context= callee_context; config_checks= callee_config_checks}
      location ({context= caller_context; config_checks= caller_config_checks} as astate) =
    let context =
      Context.instantiate_callee ~callee_pname:callee location ~callee_context ~caller_context
    in
    let config_checks =
      ConfigChecks.fold
        (fun (config, _) {context= callee_context; trace= config_check_trace} acc ->
          let context =
            Context.instantiate_callee ~callee_pname:callee ~config_check_trace location
              ~callee_context ~caller_context
          in
          let trace =
            Trace.add_call callee location ~from:Trace.Empty ~callee_trace:config_check_trace
          in
          ConfigChecks.weak_add config {context; trace} location acc )
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
    | Call (_, Const (Cfun callee), _ :: (Const (Cint marker), _) :: _, location, _)
      when FbGKInteraction.is_marker_start tenv callee ->
        Dom.call_marker_start marker location astate
    | Call (_, Const (Cfun callee), _ :: (Const (Cint marker), _) :: _, _, _)
      when FbGKInteraction.is_marker_end tenv callee ->
        Dom.call_marker_end marker astate
    | Call (_, Const (Cfun callee), _ :: (Var id, _) :: _, location, _)
      when FbGKInteraction.is_config_check tenv callee ->
        Dom.call_config_check id location astate
    | Call (_, Const (Cfun callee), _, location, _) ->
        Option.value_map (get_summary callee) ~default:astate ~f:(fun callee_summary ->
            Dom.instantiate_callee ~callee ~callee_summary location astate )
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
