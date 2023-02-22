(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** {2 Analysis API} *)

val html_debug_new_node_session :
     ?kind:[`ComputePre | `ExecNode | `ExecNodeNarrowing | `WTO]
  -> pp_name:(Format.formatter -> unit)
  -> Procdesc.Node.t
  -> f:(unit -> 'a)
  -> 'a
(** set to [NodePrinter.with_session] *)

(** {2 Callbacks management}*)

(** These callbacks are used to break the dependency cycles between some modules. Specifically, we
    put here functions needed for the analysis that depend on modules higher up the dependency graph
    than this library but whose type does not. *)
type callbacks =
  { html_debug_new_node_session_f:
      'a.
         ?kind:[`ComputePre | `ExecNode | `ExecNodeNarrowing | `WTO]
      -> pp_name:(Format.formatter -> unit)
      -> Procdesc.Node.t
      -> f:(unit -> 'a)
      -> 'a }

val set_callbacks : callbacks -> unit
(** make sure this is called before starting any actual analysis *)
