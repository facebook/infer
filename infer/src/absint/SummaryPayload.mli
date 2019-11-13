(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module type Payload = sig
  type t

  val field : (Payloads.t, t option) Field.t
end

module type S = sig
  type t

  val update_summary : t -> Summary.t -> Summary.t
  (** Update the corresponding part of the payload in the procedure summary *)

  val of_summary : Summary.t -> t option
  (** Read the corresponding part of the payload from the procedure summary *)

  val read_full : caller_summary:Summary.t -> callee_pname:Typ.Procname.t -> (Procdesc.t * t) option
  (** Return the proc desc and payload for the given procedure. Runs the analysis on-demand if
     necessary. *)

  val read : caller_summary:Summary.t -> callee_pname:Typ.Procname.t -> t option
  (** Return the payload for the given procedure. Runs the analysis on-demand if necessary. *)

  val read_toplevel_procedure : Typ.Procname.t -> t option
end

module Make (P : Payload) : S with type t = P.t
