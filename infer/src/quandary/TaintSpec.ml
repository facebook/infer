(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** combination of a trace with functions for handling unknown code and converting to and from
    summaries *)

module type S = sig
  module Trace : Trace.S

  (** return a summary for handling an unknown call at the given site with the given return type
      and actuals *)
  val handle_unknown_call : CallSite.t -> Typ.t option -> (Exp.t * Typ.t) list -> QuandarySummary.t

  (** convert a trace type into a summary trace. can be killed if we functorize specs.ml *)
  val to_summary_trace : Trace.t -> QuandarySummary.summary_trace

  (** convert summary trace into a trace type. can be killed if we functorized specs.ml *)
  val of_summary_trace : QuandarySummary.summary_trace -> Trace.t
end
