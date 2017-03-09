(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module type Helper = sig
  type summary

  (** update the specs payload with [summary] *)
  val update_payload : summary -> Specs.payload -> Specs.payload

  (** extract [summmary] from the specs payload *)
  val read_from_payload : Specs.payload -> summary option
end

module type S = sig
  type summary

  (** Write the [summary] for the procname to persistent storage. Returns the summary actually
      written. *)
  val write_summary : Typ.Procname.t -> summary -> unit

  (** read and return the summary for [callee_pname] called from [caller_pdesc]. does the analysis
      to create the summary if needed *)
  val read_summary : Procdesc.t -> Typ.Procname.t -> summary option

end

module Make (H : Helper) : S with type summary = H.summary
