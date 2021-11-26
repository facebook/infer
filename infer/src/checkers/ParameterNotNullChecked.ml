(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

let is_block_param formals_not_captured name =
  List.exists
    ~f:(fun (formal, typ) -> Mangled.equal formal name && Typ.is_pointer_to_function typ)
    formals_not_captured


module DomainData = struct
  type t = {arg: Mangled.t} [@@deriving compare]

  let pp fmt {arg} = F.fprintf fmt "%a" Mangled.pp arg
end

module BlockParams = struct
  include AbstractDomain.FiniteSet (Mangled)
end

module Vars = struct
  include PrettyPrintable.MakePPMonoMap (Ident) (DomainData)

  let compare = compare DomainData.compare
end

module TraceData = struct
  type data_usage = Assign | CheckNil | Execute | Parameter of Procname.t [@@deriving compare]

  type t = {arg: Mangled.t; loc: Location.t; usage: data_usage} [@@deriving compare]

  let to_string arg usage =
    match usage with
    | Assign ->
        "Assigned"
    | CheckNil ->
        F.asprintf "Checking `%a` for nil" Mangled.pp arg
    | Execute ->
        F.asprintf "Executing `%a`" Mangled.pp arg
    | Parameter proc_name ->
        F.asprintf "Parameter `%a` of %a" Mangled.pp arg Procname.pp proc_name


  let pp fmt {arg; loc; usage} =
    F.fprintf fmt "%a at %a, usage is %s" Mangled.pp arg Location.pp loc (to_string arg usage)
end

module TraceInfo = struct
  type t = TraceData.t list [@@deriving compare]

  let pp = Pp.semicolon_seq TraceData.pp
end

module Mem = struct
  type t = {vars: Vars.t; blockParams: BlockParams.t; traceInfo: TraceInfo.t [@compare.ignore]}
  [@@deriving compare]

  let pp fmt {vars; blockParams; traceInfo} =
    F.fprintf fmt "Vars= %a@.BlockParams= %a@.TraceInfo= %a" Vars.pp vars BlockParams.pp blockParams
      TraceInfo.pp traceInfo


  let find_block_param id (astate : t) = Vars.find_opt id astate.vars

  let exec_null_check_id id loc ({vars; blockParams; traceInfo} as astate : t) =
    match find_block_param id astate with
    | Some {arg} ->
        let traceInfo = {TraceData.arg; loc; usage= CheckNil} :: traceInfo in
        let blockParams = BlockParams.add arg blockParams in
        let vars = Vars.add id {arg} vars in
        {vars; blockParams; traceInfo}
    | None ->
        astate


  let load formals_not_captured id pvar _ astate =
    let name = Pvar.get_name pvar in
    let vars =
      if is_block_param formals_not_captured name then Vars.add id {arg= name} astate.vars else astate.vars
    in
    {astate with vars}


  let store pvar e loc ({vars; blockParams; traceInfo} as astate) =
    let name = Pvar.get_name pvar in
    let traceInfo = {TraceData.arg= name; loc; usage= Assign} :: traceInfo in
    let blockParams = BlockParams.remove name blockParams in
    let blockParams =
      match e with
      | Exp.Var id -> (
        match find_block_param id astate with
        | Some {DomainData.arg} ->
            if BlockParams.mem arg blockParams then BlockParams.add name blockParams
            else blockParams
        | None ->
            blockParams )
      | _ ->
          blockParams
    in
    {vars; blockParams; traceInfo}


  let call_trace name loc traceInfo = {TraceData.arg= name; loc; usage= Execute} :: traceInfo

  let make_trace var traceInfo =
    List.fold_left
      ~f:(fun trace_elems {TraceData.arg; loc; usage} ->
        if Mangled.equal var arg then
          let trace_elem = Errlog.make_trace_element 0 loc (TraceData.to_string arg usage) [] in
          trace_elem :: trace_elems
        else trace_elems )
      ~init:[] traceInfo


  let report_unchecked_block_param_issues proc_desc err_log var loc
      ({blockParams; traceInfo} as astate : t) =
    match find_block_param var astate with
    | Some {DomainData.arg} ->
        let traceInfo = call_trace arg loc traceInfo in
        if BlockParams.mem arg blockParams then ()
        else
          let ltr = make_trace arg traceInfo in
          let message =
            F.asprintf
              "The block `%a` is executed without a check for nil at %a. This could cause a crash."
              Mangled.pp arg Location.pp loc
          in
          Reporting.log_issue proc_desc err_log ~ltr ~loc ParameterNotNullChecked
            IssueType.block_parameter_not_null_checked message ;
          ()
    | None ->
        ()
end

module Domain = struct
  include AbstractDomain.FiniteSet (Mem)

  let exec_null_check_id id loc astate = map (Mem.exec_null_check_id id loc) astate

  let load formals_not_captured id pvar loc astate = map (Mem.load formals_not_captured id pvar loc) astate

  let store pvar e loc astate = map (Mem.store pvar e loc) astate

  let report_unchecked_block_param_issues proc_desc err_log var loc astate =
    iter (Mem.report_unchecked_block_param_issues proc_desc err_log var loc) astate
end

module TransferFunctions = struct
  module Domain = Domain
  module CFG = ProcCfg.Normal

  type analysis_data = IntraproceduralAnalysis.t * (Mangled.t * Typ.t) list

  let pp_session_name _node fmt = F.pp_print_string fmt "ParameterNotNullChecked"

  let exec_instr (astate : Domain.t)
      ({IntraproceduralAnalysis.proc_desc; err_log}, formals_not_captured) _cfg_node _
      (instr : Sil.instr) =
    match instr with
    | Load {id; e= Lvar pvar; loc} ->
        Domain.load formals_not_captured id pvar loc astate
    | Store {e1= Lvar pvar; e2; loc} ->
        Domain.store pvar e2 loc astate
    | Prune (Var id, loc, _, _) ->
        Domain.exec_null_check_id id loc astate
    (* If (block != nil) or equivalent else branch *)
    | Prune (BinOp (Binop.Ne, e1, e2), loc, _, _)
    (* If (!(block == nil)) or equivalent else branch *)
    | Prune (UnOp (LNot, BinOp (Binop.Eq, e1, e2), _), loc, _, _) -> (
      match (e1, e2) with
      | Var id, e when Exp.is_null_literal e ->
          Domain.exec_null_check_id id loc astate
      | e, Var id when Exp.is_null_literal e ->
          Domain.exec_null_check_id id loc astate
      | _ ->
          astate )
    | Call (_, Exp.Var var, _, loc, call_flags) when call_flags.CallFlags.cf_is_objc_block ->
        Domain.report_unchecked_block_param_issues proc_desc err_log var loc astate ;
        astate
    | _ ->
        astate
end

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

let formals_not_captured attributes =
  let is_not_captured (formal, _) =
    not
      (List.exists
         ~f:(fun ({name} : CapturedVar.t) -> Mangled.equal formal name)
         attributes.ProcAttributes.captured )
  in
  List.filter ~f:is_not_captured attributes.ProcAttributes.formals


let init_block_params attributes formals_not_captured initBlockParams =
  let add_non_nullable_block blockParams annotation (formal, _) =
    if is_block_param formals_not_captured formal && Annotations.ia_is_nonnull annotation then
      BlockParams.add formal blockParams
    else blockParams
  in
  let annotations = attributes.ProcAttributes.method_annotation.params in
  match
    List.fold2 annotations formals_not_captured ~init:initBlockParams ~f:add_non_nullable_block
  with
  | List.Or_unequal_lengths.Ok blockParams ->
      blockParams
  | List.Or_unequal_lengths.Unequal_lengths ->
      initBlockParams


let init_block_param_trace_info attributes formals_not_captured traceInfo =
  let add_trace_info_block traceInfo (formal, _) =
    if is_block_param formals_not_captured formal then
      let usage = TraceData.Parameter attributes.ProcAttributes.proc_name in
      {TraceData.arg= formal; loc= attributes.ProcAttributes.loc; usage} :: traceInfo
    else traceInfo
  in
  List.fold formals_not_captured ~init:traceInfo ~f:add_trace_info_block


let checker ({IntraproceduralAnalysis.proc_desc} as analysis_data') =
  let attributes = Procdesc.get_attributes proc_desc in
  let formals_not_captured = formals_not_captured attributes in
  let initial_blockParams = init_block_params attributes formals_not_captured BlockParams.empty in
  let initTraceInfo = init_block_param_trace_info attributes formals_not_captured [] in
  let initial =
    Domain.singleton
      {Mem.vars= Vars.empty; blockParams= initial_blockParams; traceInfo= initTraceInfo}
  in
  let analysis_data = (analysis_data', formals_not_captured) in
  ignore (Analyzer.compute_post analysis_data ~initial proc_desc)
