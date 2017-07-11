(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Simplified html node printer for checkers *)

val start_session : Procdesc.Node.t -> unit
(** To be called before analyzing a node *)

val finish_session : Procdesc.Node.t -> unit
(** To be called after analyzing a node *)
