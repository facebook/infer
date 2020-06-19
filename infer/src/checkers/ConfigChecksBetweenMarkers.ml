(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open AbstractDomain.Types
module F = Format
module ConfigName = FbGKInteraction.ConfigName

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

module Loc = struct
  type t = Id of Ident.t | Pvar of Pvar.t [@@deriving compare]

  let pp f = function Id id -> Ident.pp f id | Pvar pvar -> Pvar.pp Pp.text f pvar

  let of_id id = Id id

  let of_pvar pvar = Pvar pvar
end

module Mem = struct
  include AbstractDomain.Map (Loc) (ConfigTopLifted)

  let copy ~to_ ~from mem =
    Option.value_map (find_opt from mem) ~default:mem ~f:(fun config -> add to_ config mem)
end

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
  let join x _y = x

  let widen ~prev ~next ~num_iters:_ = join prev next

  let make_desc = function
    | MarkerStart marker ->
        F.asprintf "Marker %a start" IntLit.pp marker
    | ConfigCheck gk_switch ->
        F.asprintf "Config %a is checked" ConfigName.pp gk_switch


  let call_desc callee = F.asprintf "Function %a is called" Procname.pp callee

  let marker_start marker = MarkerStart marker

  let config_check config = ConfigCheck config

  let add_elem kind location from = Elem {kind; location; from}

  let add_call callee location ~from ~callee_trace = Call {callee; location; from; callee_trace}

  let singleton kind location = add_elem kind location Empty

  let make_err_trace x =
    let rec make_err_trace ~depth x acc ~cont =
      match x with
      | Empty ->
          cont acc
      | Elem {kind; location; from} ->
          let acc = Errlog.make_trace_element depth location (make_desc kind) [] :: acc in
          make_err_trace ~depth from acc ~cont
      | Call {callee; location; from; callee_trace} ->
          make_err_trace ~depth:(depth + 1) callee_trace acc ~cont:(fun acc ->
              let acc = Errlog.make_trace_element depth location (call_desc callee) [] :: acc in
              make_err_trace ~depth from acc ~cont )
    in
    make_err_trace ~depth:0 x [] ~cont:Fn.id
end

module Reported = AbstractDomain.BooleanOr

module TraceWithReported = struct
  type t = {trace: Trace.t; reported: Reported.t}

  let pp f {trace; reported} =
    F.fprintf f "@[@[trace:@ %a@]@\n@[reported:@ %a@]@]" Trace.pp trace Reported.pp reported


  let leq ~lhs ~rhs = Reported.leq ~lhs:lhs.reported ~rhs:rhs.reported

  let join x y = if leq ~lhs:x ~rhs:y then y else x

  let widen ~prev ~next ~num_iters:_ = join prev next
end

module MarkerSet = struct
  include AbstractDomain.Map (IntLit) (TraceWithReported)

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
    map
      (fun ({trace} as trace_with_reported) ->
        {trace_with_reported with trace= Trace.add_elem new_trace location trace} )
      x


  let join_on_call callee_pname ?config_check_trace location ~callee ~caller =
    merge
      (fun _marker callee_trace caller_trace ->
        let add_call {TraceWithReported.trace= callee_trace; reported= callee_reported} =
          let from, caller_reported =
            Option.value_map caller_trace ~default:(Trace.Empty, false)
              ~f:(fun {TraceWithReported.trace; reported} -> (trace, reported))
          in
          let trace = Trace.add_call callee_pname location ~from ~callee_trace in
          Some {TraceWithReported.trace; reported= Reported.join callee_reported caller_reported}
        in
        match (callee_trace, config_check_trace) with
        | None, None ->
            caller_trace
        | None, Some config_check_trace ->
            add_call {TraceWithReported.trace= config_check_trace; reported= false}
        | Some callee_trace, _ ->
            add_call callee_trace )
      callee caller


  let report {InterproceduralAnalysis.proc_desc; err_log} config location markers =
    let report_on_marker marker ({TraceWithReported.trace; reported} as trace_reported) =
      if reported then trace_reported
      else
        let desc =
          F.asprintf "Config %a is checked inside marker %a" ConfigName.pp config IntLit.pp marker
        in
        Reporting.log_issue proc_desc err_log ~loc:location ~ltr:(Trace.make_err_trace trace)
          ConfigChecksBetweenMarkers IssueType.config_checks_between_markers desc ;
        {TraceWithReported.trace; reported= true}
    in
    mapi report_on_marker markers
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
    let trace_with_reported = {TraceWithReported.trace; reported= false} in
    { started_markers= MarkerSet.add marker trace_with_reported started_markers
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


  let load_constant_config id config ({mem} as astate) =
    {astate with mem= Mem.add id (NonTop config) mem}


  let mem_copy ~to_ ~from ({mem} as astate) = {astate with mem= Mem.copy ~to_ ~from mem}

  let load_config id pvar astate = mem_copy ~to_:(Loc.of_id id) ~from:(Loc.of_pvar pvar) astate

  let store_config pvar id astate = mem_copy ~to_:(Loc.of_pvar pvar) ~from:(Loc.of_id id) astate

  let call_marker_start marker location ({context} as astate) =
    {astate with context= Context.call_marker_start marker location context}


  let call_marker_end marker ({context} as astate) =
    {astate with context= Context.call_marker_end marker context}


  let call_config_check analysis_data id location ({mem; context; config_checks} as astate) =
    match Mem.find_opt (Loc.of_id id) mem with
    | Some (NonTop config) ->
        let trace_elem = Trace.config_check config in
        let context = Context.call_config_check trace_elem location context in
        let context =
          { context with
            started_markers= MarkerSet.report analysis_data config location context.started_markers
          }
        in
        let context_with_trace =
          {ContextWithTrace.context; trace= Trace.singleton trace_elem location}
        in
        { astate with
          config_checks= ConfigChecks.weak_add config context_with_trace location config_checks }
    | _ ->
        astate


  let instantiate_callee analysis_data ~callee
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
          let context =
            { context with
              started_markers=
                MarkerSet.report analysis_data config location context.started_markers }
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

module TransferFunctions = struct
  module CFG = ProcCfg.NormalOneInstrPerNode
  module Domain = Dom

  type analysis_data = Summary.t InterproceduralAnalysis.t

  let exec_instr astate ({InterproceduralAnalysis.tenv; analyze_dependency} as analysis_data) _node
      instr =
    match (instr : Sil.instr) with
    | Load {id; e} -> (
      match FbGKInteraction.get_config e with
      | Some config ->
          Dom.load_constant_config (Loc.of_id id) config astate
      | None -> (
        match e with Lvar pvar -> Dom.load_config id pvar astate | _ -> astate ) )
    | Store {e1= Lvar pvar; e2= Exp.Var id} ->
        Dom.store_config pvar id astate
    | Call (_, Const (Cfun callee), _ :: (Const (Cint marker), _) :: _, location, _)
      when FbGKInteraction.is_marker_start tenv callee ->
        Dom.call_marker_start marker location astate
    | Call (_, Const (Cfun callee), _ :: (Const (Cint marker), _) :: _, _, _)
      when FbGKInteraction.is_marker_end tenv callee ->
        Dom.call_marker_end marker astate
    | Call (_, Const (Cfun callee), args, location, _) -> (
      match FbGKInteraction.get_config_check tenv callee args with
      | Some id ->
          Dom.call_config_check analysis_data id location astate
      | None ->
          Option.value_map (analyze_dependency callee) ~default:astate
            ~f:(fun (_, callee_summary) ->
              Dom.instantiate_callee analysis_data ~callee ~callee_summary location astate ) )
    | _ ->
        astate


  let pp_session_name node fmt =
    F.fprintf fmt "Config checks between markers %a" CFG.Node.pp_id (CFG.Node.id node)
end

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) =
  Option.map (Analyzer.compute_post analysis_data ~initial:Dom.init proc_desc) ~f:Dom.to_summary
