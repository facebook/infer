(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

module Payload = SummaryPayload.Make (struct
  type t = ClassLoadsDomain.summary

  let update_payloads post (payloads : Payloads.t) = {payloads with class_loads= Some post}

  let of_payloads (payloads : Payloads.t) = payloads.class_loads
end)

let exec_instr pdesc astate _ (instr : Sil.instr) =
  match instr with
  | Call (_, Const (Cfun callee), _, loc, _) ->
      Payload.read pdesc callee
      |> Option.fold ~init:astate ~f:(ClassLoadsDomain.integrate_summary callee loc)
  | _ ->
      astate


let analyze_procedure {Callbacks.proc_desc; summary} =
  let init = ClassLoadsDomain.empty in
  let post = Procdesc.fold_instrs proc_desc ~init ~f:(exec_instr proc_desc) in
  Payload.update_summary post summary
