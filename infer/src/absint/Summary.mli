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
  (** Uptade the corresponding part of the payload in the procedure summary *)

  val read_payload : Specs.summary -> payload option
  (** Read the corresponding part of the payload from the procedure summary *)
end

module type S = sig
  type payload

  val update_summary : payload -> Specs.summary -> Specs.summary
  (** Uptade the corresponding part of the payload in the procedure summary *)

  val read_summary : Procdesc.t -> Typ.Procname.t -> payload option
  (** Return the payload for the given procedure.
      Runs the analysis on-demand if necessary *)
end

module Make (P : Payload) : S with type payload = P.payload
