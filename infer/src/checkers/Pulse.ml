(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
open Result.Monad_infix

let read astate access_expr =
  PulseDomain.materialize_location astate access_expr
  >>= fun (astate, loc) -> PulseDomain.check_loc_access loc astate


let read_all access_exprs astate = List.fold_result access_exprs ~init:astate ~f:read

let write access_expr astate =
  PulseDomain.overwrite_location astate access_expr (PulseDomain.AbstractLocation.mk_fresh ())
  >>| fun (astate, _) -> astate


let check_error summary loc = function
  | Ok astate ->
      astate
  | Error (astate, message) ->
      Reporting.log_error summary ~loc IssueType.use_after_lifetime message ;
      astate


let invalidate access_expr astate =
  PulseDomain.materialize_location astate access_expr
  >>= fun (astate, loc) -> PulseDomain.check_loc_access loc astate >>| PulseDomain.mark_invalid loc


module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = PulseDomain

  type extras = Summary.t

  let exec_instr (astate : Domain.astate) {ProcData.extras= summary} _cfg_node (instr : HilInstr.t)
      =
    match instr with
    | Assign (lhs_access, rhs_exp, loc) ->
        (* we could be more precise and try and evaluate [rhs_exp] down to a location and use it to
           record the value written instead of recording a fresh location *)
        write lhs_access astate
        >>= read_all (HilExp.get_access_exprs rhs_exp)
        |> check_error summary loc
    | Assume (condition, _, _, loc) ->
        read_all (HilExp.get_access_exprs condition) astate |> check_error summary loc
    | Call (_ret, HilInstr.Direct callee_pname, [AccessExpression deleted_access], _flags, loc)
      when Typ.Procname.equal callee_pname BuiltinDecl.__delete ->
        (* TODO: use {!ProcnameDispatcher.ProcName} instead of pattern matching name ourselves *)
        invalidate deleted_access astate |> check_error summary loc
    | Call (_ret, HilInstr.Direct _, actuals, _flags, loc)
    | Call (_ret, HilInstr.Indirect _, actuals, _flags, loc) ->
        (* TODO: function calls, right now we just register the reads of the arguments *)
        read_all (List.concat_map actuals ~f:HilExp.get_access_exprs) astate
        |> check_error summary loc


  let pp_session_name _node fmt = F.pp_print_string fmt "Pulse"
end

module Analyzer = LowerHil.MakeAbstractInterpreter (ProcCfg.Exceptional) (TransferFunctions)

let checker {Callbacks.proc_desc; tenv; summary} =
  let proc_data = ProcData.make proc_desc tenv summary in
  ignore (Analyzer.compute_post proc_data ~initial:PulseDomain.initial) ;
  summary
