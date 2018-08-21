(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module type Payload = sig
  type t

  val update_payloads : t -> Payloads.t -> Payloads.t
  (** Update the corresponding part of the payloads *)

  val of_payloads : Payloads.t -> t option
  (** Read the corresponding part of the payloads *)
end

module type S = sig
  type t

  val update_summary : t -> Summary.t -> Summary.t
  (** Update the corresponding part of the payload in the procedure summary *)

  val of_summary : Summary.t -> t option
  (** Read the corresponding part of the payload from the procedure summary *)

  val read : Procdesc.t -> Typ.Procname.t -> t option
  (** Return the payload for the given procedure. Runs the analysis on-demand if necessary. *)
end

module Make (P : Payload) : S with type t = P.t
