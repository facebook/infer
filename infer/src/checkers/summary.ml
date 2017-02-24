(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module type Helper = sig
  type summary
  (*  type astate*)

  (** update the specs payload with [summary] *)
  val update_payload : summary -> Specs.payload -> Specs.payload

  (** extract [summmary] from the specs payload *)
  val read_from_payload : Specs.payload -> summary option
end

module type S = sig
  type summary
  (*  type astate*)

  (** Write the [summary] for the procname to persistent storage. Returns the summary actually
      written. *)
  val write_summary : Procname.t -> summary -> unit

  (** read and return the summary for [callee_pname] called from [caller_pdesc]. does the analysis
      to create the summary if needed *)
  val read_summary : Procdesc.t -> Procname.t -> summary option
end

module Make (H : Helper) = struct
  type summary = H.summary

  let write_summary pname summary =
    match Specs.get_summary pname with
    | Some global_summary ->
        let payload = H.update_payload summary global_summary.Specs.payload in
        let timestamp = global_summary.timestamp + 1 in
        Specs.store_summary pname { global_summary with payload; timestamp; }
    | None ->
        failwithf "Summary for %a should exist, but does not!@." Procname.pp pname

  let read_summary caller_pdesc callee_pname =
    ignore (Ondemand.analyze_proc_name ~propagate_exceptions:false caller_pdesc callee_pname);
    match Specs.get_summary callee_pname with
    | None -> None
    | Some summary -> H.read_from_payload summary.Specs.payload
end
