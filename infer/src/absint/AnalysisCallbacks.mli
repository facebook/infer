(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** {2 Analysis API} *)

val get_proc_desc : Procname.t -> Procdesc.t option
(** set to [Ondemand.get_proc_desc] *)

val html_debug_new_node_session :
     ?kind:[`ComputePre | `ExecNode | `ExecNodeNarrowing | `WTO]
  -> pp_name:(Format.formatter -> unit)
  -> Procdesc.Node.t
  -> f:(unit -> 'a)
  -> 'a
(** set to [NodePrinter.with_session] *)

val proc_resolve_attributes : Procname.t -> ProcAttributes.t option
(** set to [Summary.OnDisk.proc_resolve_attributes] *)

(** {2 Callbacks management}*)

(** These callbacks are used to break the dependency cycles between some modules. Specifically, we
    put here functions needed for the analysis that depend on modules higher up the dependency graph
    than this library but whose type does not. *)
type callbacks =
  { get_proc_desc_f: Procname.t -> Procdesc.t option
  ; html_debug_new_node_session_f:
      'a.    ?kind:[`ComputePre | `ExecNode | `ExecNodeNarrowing | `WTO]
      -> pp_name:(Format.formatter -> unit) -> Procdesc.Node.t -> f:(unit -> 'a) -> 'a
  ; proc_resolve_attributes_f: Procname.t -> ProcAttributes.t option }

val set_callbacks : callbacks -> unit
(** make sure this is called before starting any actual analysis *)
