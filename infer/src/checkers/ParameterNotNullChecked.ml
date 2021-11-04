(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

let is_block_param attributes name =
  List.exists
    ~f:(fun (formal, typ) -> Mangled.equal formal name && Typ.is_pointer_to_function typ)
    attributes.ProcAttributes.formals


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

module Mem = struct
  type t = {vars: Vars.t; blockParams: BlockParams.t} [@@deriving compare]

  let pp fmt {vars; blockParams} = F.fprintf fmt "%a@.%a" Vars.pp vars BlockParams.pp blockParams

  let find_block_param id (astate : t) = Vars.find_opt id astate.vars

  let exec_null_check_id id ({vars; blockParams} as astate : t) =
    match find_block_param id astate with
    | Some elem ->
        let blockParams = BlockParams.add elem.arg blockParams in
        {vars; blockParams}
    | None ->
        astate


  let report_unchecked_block_param_issues proc_desc err_log var loc ({blockParams} as astate : t) =
    match find_block_param var astate with
    | Some {DomainData.arg} ->
        if BlockParams.mem arg blockParams then ()
        else
          let message =
            F.asprintf
              "The block `%a` is executed without a check for null at %a. This could cause a crash."
              Mangled.pp arg Location.pp loc
          in
          Reporting.log_issue proc_desc err_log ~loc ParameterNotNullChecked
            IssueType.block_parameter_not_null_checked message ;
          ()
    | None ->
        ()


  let load attributes id pvar astate =
    let name = Pvar.get_name pvar in
    let vars =
      if is_block_param attributes name then Vars.add id {arg= name} astate.vars else astate.vars
    in
    {astate with vars}
end

module Domain = struct
  include AbstractDomain.FiniteSet (Mem)

  let exec_null_check_id id astate = map (Mem.exec_null_check_id id) astate

  let report_unchecked_block_param_issues proc_desc err_log var loc astate =
    iter (Mem.report_unchecked_block_param_issues proc_desc err_log var loc) astate


  let load attributes id pvar astate = map (Mem.load attributes id pvar) astate
end

module TransferFunctions = struct
  module Domain = Domain
  module CFG = ProcCfg.Normal

  type analysis_data = IntraproceduralAnalysis.t

  let pp_session_name _node fmt = F.pp_print_string fmt "ParameterNotNullChecked"

  let exec_instr (astate : Domain.t) {IntraproceduralAnalysis.proc_desc; err_log} _cfg_node _
      (instr : Sil.instr) =
    let attributes = Procdesc.get_attributes proc_desc in
    match instr with
    | Load {id; e= Lvar pvar} ->
        Domain.load attributes id pvar astate
    | Prune (Var id, _, _, _) ->
        Domain.exec_null_check_id id astate
    (* If (block != nil) or equivalent else branch *)
    | Prune (BinOp (Binop.Ne, e1, e2), _, _, _)
    (* If (!(block == nil)) or equivalent else branch *)
    | Prune (UnOp (LNot, BinOp (Binop.Eq, e1, e2), _), _, _, _) -> (
      match (e1, e2) with
      | Var id, e when Exp.is_null_literal e ->
          Domain.exec_null_check_id id astate
      | e, Var id when Exp.is_null_literal e ->
          Domain.exec_null_check_id id astate
      | _ ->
          astate )
    | Call (_, Exp.Var var, _, loc, call_flags) when call_flags.CallFlags.cf_is_objc_block ->
        Domain.report_unchecked_block_param_issues proc_desc err_log var loc astate ;
        astate
    | _ ->
        astate
end

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

let checker ({IntraproceduralAnalysis.proc_desc} as analysis_data) =
  let initial = Domain.singleton {Mem.vars= Vars.empty; blockParams= BlockParams.empty} in
  ignore (Analyzer.compute_post analysis_data ~initial proc_desc)
