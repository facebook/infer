(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module type Payload = sig
  type payload

  val update_payload : payload -> Specs.summary -> Specs.summary

  val read_payload : Specs.summary -> payload option
end

module type S = sig
  type payload

  val update_summary : payload -> Specs.summary -> Specs.summary

  val read_summary : Procdesc.t -> Typ.Procname.t -> payload option
end

module Make (P : Payload) : S with type payload = P.payload = struct
  type payload = P.payload

  let update_summary payload summary = P.update_payload payload summary

  let read_summary caller_pdesc callee_pname =
    match Ondemand.analyze_proc_name ~propagate_exceptions:false caller_pdesc callee_pname with
    | None
     -> None
    | Some summary
     -> P.read_payload summary
end
