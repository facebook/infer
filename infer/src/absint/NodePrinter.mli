(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Simplified html node printer for checkers *)

val with_session :
     ?kind:[< `ComputePre | `ExecNode | `ExecNodeNarrowing | `WTO]
  -> pp_name:(Format.formatter -> unit)
  -> Procdesc.Node.t
  -> f:(unit -> 'a)
  -> 'a
(**
  Wraps [f] in an html debug session.
  Will swallow timeouts so do *not* use from within biabduction.
*)
