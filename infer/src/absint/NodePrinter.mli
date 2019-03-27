(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Simplified html node printer for checkers *)

val start_session : pp_name:(Format.formatter -> unit) -> Procdesc.Node.t -> unit
(** To be called before analyzing a node *)

val finish_session : Procdesc.Node.t -> unit
(** To be called after analyzing a node *)

val with_session : pp_name:(Format.formatter -> unit) -> Procdesc.Node.t -> f:(unit -> 'a) -> 'a
(** Wraps [f] between [start_session] and [finish_session]. Will swallow timeouts so do *not* use
   from within biabduction *)
