(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module ConfigName = FbGKInteraction.ConfigName
module ConfigLifted = AbstractDomain.Flat (ConfigName)

module Marker = struct
  type t = MarkerId of IntLit.t | MarkerName of {marker_class: Pvar.t; marker_name: Fieldname.t}
  [@@deriving compare]

  let equal = [%compare.equal: t]

  let pp f = function
    | MarkerId i ->
        IntLit.pp f i
    | MarkerName {marker_name} ->
        Fieldname.pp f marker_name


  let of_int_lit i = MarkerId i

  let of_name marker_class marker_name = MarkerName {marker_class; marker_name}
end

module MarkerLifted = AbstractDomain.Flat (Marker)

module ConfigWithLocation = struct
  type t = ConfigName.t * Location.t [@@deriving compare]

  let pp f (config, location) = F.fprintf f "%a at %a" ConfigName.pp config Location.pp location
end

module Loc = struct
  type t = Id of Ident.t | Pvar of Pvar.t | ThisField of Fieldname.t [@@deriving compare]

  let pp f = function
    | Id id ->
        Ident.pp f id
    | Pvar pvar ->
        Pvar.pp Pp.text f pvar
    | ThisField fn ->
        F.fprintf f "this.%a" Fieldname.pp fn


  let is_this = function Pvar pvar -> Pvar.is_this pvar | Id _ | ThisField _ -> false

  let of_id id = Id id

  let of_pvar pvar = Pvar pvar

  let of_this_field fn = ThisField fn
end

module Locs = AbstractDomain.FiniteSet (Loc)

module Val = struct
  (* NOTE: Instead of syntactically distinguishing config and marker variables with heuristics, we
     evalute the values for both of them if possible. Later, one value of them should be actually
     used in analyzing config checking or marker start/end statments. *)
  type t = {config: ConfigLifted.t; marker: MarkerLifted.t; locs: Locs.t}

  let pp f {config; marker; locs} =
    F.fprintf f "@[@[config:@ %a@]@\n@[marker:@ %a@]@\n@[locs:@ %a@]@]" ConfigLifted.pp config
      MarkerLifted.pp marker Locs.pp locs


  let leq ~lhs ~rhs =
    ConfigLifted.leq ~lhs:lhs.config ~rhs:rhs.config
    && MarkerLifted.leq ~lhs:lhs.marker ~rhs:rhs.marker
    && Locs.leq ~lhs:lhs.locs ~rhs:rhs.locs


  let join x y =
    { config= ConfigLifted.join x.config y.config
    ; marker= MarkerLifted.join x.marker y.marker
    ; locs= Locs.join x.locs y.locs }


  let widen ~prev ~next ~num_iters =
    { config= ConfigLifted.widen ~prev:prev.config ~next:next.config ~num_iters
    ; marker= MarkerLifted.widen ~prev:prev.marker ~next:next.marker ~num_iters
    ; locs= Locs.widen ~prev:prev.locs ~next:next.locs ~num_iters }


  let make ?(config = ConfigLifted.bottom) ?(marker = MarkerLifted.bottom) ?(locs = Locs.bottom) ()
      =
    {config; marker; locs}


  let of_config config = make ~config:(ConfigLifted.v config) ()

  let of_marker marker = make ~marker:(MarkerLifted.v marker) ()

  let of_loc loc = make ~locs:(Locs.singleton loc) ()

  let is_bottom {config; marker; locs} =
    ConfigLifted.is_bottom config && MarkerLifted.is_bottom marker && Locs.is_bottom locs


  let get_config_opt {config} = ConfigLifted.get config

  let get_marker_opt {marker} = MarkerLifted.get marker

  let get_locs {locs} = locs
end

module Mem = struct
  include AbstractDomain.Map (Loc) (Val)

  let add l v mem = if Val.is_bottom v then mem else add l v mem

  let get_config_opt l mem = Option.bind (find_opt l mem) ~f:Val.get_config_opt

  let get_marker_opt l mem = Option.bind (find_opt l mem) ~f:Val.get_marker_opt

  let load id pvar mem =
    let from = Loc.of_pvar pvar in
    let v = IOption.value_default_f (find_opt from mem) ~f:(fun () -> Val.of_loc from) in
    add (Loc.of_id id) v mem


  let store pvar id mem =
    Option.value_map
      (find_opt (Loc.of_id id) mem)
      ~default:mem
      ~f:(fun v -> add (Loc.of_pvar pvar) v mem)


  let store_constant id fn marker mem =
    let marker = Marker.of_int_lit marker in
    let add_marker loc acc =
      if Loc.is_this loc then add (Loc.of_this_field fn) (Val.of_marker marker) acc else acc
    in
    let locs =
      find_opt (Loc.of_id id) mem |> Option.value_map ~default:Locs.bottom ~f:Val.get_locs
    in
    Locs.fold add_marker locs mem
end

module Trace = struct
  type elem = MarkerStart of Marker.t | ConfigCheck of ConfigName.t

  type t =
    | Empty
    | Elem of {kind: elem; location: Location.t; from: t}
    | Call of {callee: Procname.t; location: Location.t; from: t; callee_trace: t}

  let pp_elem f = function
    | MarkerStart marker ->
        F.fprintf f "MarkerStart(%a)" Marker.pp marker
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
        F.asprintf "Marker %a start" Marker.pp marker
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
  include AbstractDomain.Map (Marker) (TraceWithReported)

  let pp f x =
    if is_empty x then F.pp_print_string f "{ }"
    else (
      F.fprintf f "@[{ " ;
      let is_first = ref true in
      iter
        (fun marker _trace ->
          if !is_first then is_first := false else F.fprintf f ",@ " ;
          Marker.pp f marker )
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
          F.asprintf "Config %a is checked inside marker %a" ConfigName.pp config Marker.pp marker
        in
        Reporting.log_issue proc_desc err_log ~loc:location ~ltr:(Trace.make_err_trace trace)
          ConfigChecksBetweenMarkers IssueType.config_checks_between_markers desc ;
        {TraceWithReported.trace; reported= true}
    in
    mapi report_on_marker markers
end

module InvMarkerSet = AbstractDomain.InvertedSet (Marker)

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
  type t = {context: Context.t; config_checks: ConfigChecks.t; mem: Mem.t}

  let pp f {context; config_checks; mem} =
    F.fprintf f "@[@[%a@]@\n@[config checks:@ %a@]@\n@[mem:@ %a@]@]" Context.pp context
      ConfigChecks.pp config_checks Mem.pp mem
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


  let load_constant id config marker ({mem} as astate) =
    {astate with mem= Mem.add id (Val.make ~config ~marker ()) mem}


  let load_constant_config id config ({mem} as astate) =
    {astate with mem= Mem.add id (Val.of_config config) mem}


  let load_config id pvar ({mem} as astate) = {astate with mem= Mem.load id pvar mem}

  let store_config pvar id ({mem} as astate) = {astate with mem= Mem.store pvar id mem}

  let store_constant id fn marker ({mem} as astate) =
    {astate with mem= Mem.store_constant id fn marker mem}


  let call_marker_start marker location ({context} as astate) =
    {astate with context= Context.call_marker_start marker location context}


  let call_marker_start_id id location ({mem} as astate) =
    Mem.get_marker_opt (Loc.of_id id) mem
    |> Option.value_map ~default:astate ~f:(fun marker -> call_marker_start marker location astate)


  let call_marker_end marker ({context} as astate) =
    {astate with context= Context.call_marker_end marker context}


  let call_marker_end_id id ({mem} as astate) =
    Mem.get_marker_opt (Loc.of_id id) mem
    |> Option.value_map ~default:astate ~f:(fun marker -> call_marker_end marker astate)


  let call_config_check analysis_data config location ({context; config_checks} as astate) =
    let trace_elem = Trace.config_check config in
    let context = Context.call_config_check trace_elem location context in
    let context =
      { context with
        started_markers= MarkerSet.report analysis_data config location context.started_markers }
    in
    let context_with_trace =
      {ContextWithTrace.context; trace= Trace.singleton trace_elem location}
    in
    { astate with
      config_checks= ConfigChecks.weak_add config context_with_trace location config_checks }


  let call_config_check_exp analysis_data e location ({mem} as astate) =
    let astate' =
      let open IOption.Let_syntax in
      let* loc =
        match (e : Exp.t) with
        | Var id ->
            Some (Loc.of_id id)
        | Lvar pvar ->
            Some (Loc.of_pvar pvar)
        | _ ->
            None
      in
      let+ config = Mem.get_config_opt loc mem in
      call_config_check analysis_data config location astate
    in
    Option.value astate' ~default:astate


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


  let to_summary {mem; context; config_checks} = {Summary.context; config_checks; mem}
end

module TransferFunctions = struct
  module CFG = ProcCfg.NormalOneInstrPerNode
  module Domain = Dom

  type analysis_data = Summary.t InterproceduralAnalysis.t

  let get_java_constructor tenv typ =
    let open IOption.Let_syntax in
    let* typ_name = Typ.name typ in
    let* {Struct.methods} = Tenv.lookup tenv typ_name in
    List.find methods ~f:Procname.is_constructor


  let get_marker_from_load {InterproceduralAnalysis.tenv; analyze_dependency} e mem =
    match e with
    | Exp.Lfield (Lvar pvar, fn, _) when Pvar.is_global pvar ->
        Some (Marker.of_name pvar fn)
    | Exp.Lfield (Var id, fn, typ) -> (
        let open IOption.Let_syntax in
        let* locs = Mem.find_opt (Loc.of_id id) mem in
        match Val.get_locs locs |> Locs.is_singleton_or_more with
        | Singleton this when Loc.is_this this ->
            let* constructor = get_java_constructor tenv typ in
            let* _, {Summary.mem= constructor_mem} = analyze_dependency constructor in
            let* v = Mem.find_opt (Loc.of_this_field fn) constructor_mem in
            Val.get_marker_opt v
        | _ ->
            None )
    | _ ->
        None


  let get_marker_from_java_param e mem =
    match e with
    | Exp.Const (Cint marker) ->
        Some (Marker.of_int_lit marker)
    | Exp.Var id ->
        Mem.get_marker_opt (Loc.of_id id) mem
    | _ ->
        None


  let exec_instr ({Dom.mem} as astate)
      ({InterproceduralAnalysis.tenv; analyze_dependency} as analysis_data) _node instr =
    match (instr : Sil.instr) with
    | Load {id; e= Lvar pvar} ->
        Dom.load_config id pvar astate
    | Load {id; e} ->
        let config =
          Option.value_map (FbGKInteraction.get_config e) ~default:ConfigLifted.bottom
            ~f:ConfigLifted.v
        in
        let marker =
          get_marker_from_load analysis_data e mem
          |> Option.value_map ~default:MarkerLifted.bottom ~f:MarkerLifted.v
        in
        Dom.load_constant (Loc.of_id id) config marker astate
    | Call (_, Const (Cfun callee), (Lvar pvar, _) :: (e, _) :: _, _, _)
      when FbGKInteraction.is_config_load callee ->
        Option.value_map (FbGKInteraction.get_config e) ~default:astate ~f:(fun config ->
            Dom.load_constant_config (Loc.of_pvar pvar) config astate )
    | Store {e1= Lvar pvar; e2= Exp.Var id} ->
        Dom.store_config pvar id astate
    | Store {e1= Lfield (Var id, fn, _); e2= Const (Const.Cint marker)} ->
        Dom.store_constant id fn marker astate
    | Call (_, Const (Cfun callee), _ :: (e, _) :: _, location, _)
      when FbGKInteraction.is_marker_start_java tenv callee ->
        get_marker_from_java_param e mem
        |> Option.value_map ~default:astate ~f:(fun marker ->
               Dom.call_marker_start marker location astate )
    | Call (_, Const (Cfun callee), _ :: (e, _) :: _, _, _)
      when FbGKInteraction.is_marker_end_java tenv callee ->
        get_marker_from_java_param e mem
        |> Option.value_map ~default:astate ~f:(fun marker -> Dom.call_marker_end marker astate)
    | Call (_, Const (Cfun callee), (Var id, _) :: _, location, _)
      when FbGKInteraction.is_marker_start_objc callee ->
        Dom.call_marker_start_id id location astate
    | Call (_, Const (Cfun callee), (Var id, _) :: _, _, _)
      when FbGKInteraction.is_marker_end_objc callee ->
        Dom.call_marker_end_id id astate
    | Call (_, Const (Cfun callee), args, location, _) -> (
      match FbGKInteraction.get_config_check tenv callee args with
      | Some (`Config config) ->
          Dom.call_config_check analysis_data config location astate
      | Some (`Exp e) ->
          Dom.call_config_check_exp analysis_data e location astate
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
