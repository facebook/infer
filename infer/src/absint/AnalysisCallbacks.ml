(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type callbacks =
  { get_proc_desc_f: Procname.t -> Procdesc.t option
  ; html_debug_new_node_session_f:
      'a.
         ?kind:[`ComputePre | `ExecNode | `ExecNodeNarrowing | `WTO]
      -> pp_name:(Format.formatter -> unit)
      -> Procdesc.Node.t
      -> f:(unit -> 'a)
      -> 'a
  ; proc_resolve_attributes_f: Procname.t -> ProcAttributes.t option }

let callbacks_ref : callbacks option ref = ref None

let set_callbacks callbacks = callbacks_ref := Some callbacks

let get_callbacks () =
  match !callbacks_ref with
  | Some callbacks ->
      callbacks
  | None ->
      L.die InternalError "Callbacks not set"


let get_proc_desc proc_name = (get_callbacks ()).get_proc_desc_f proc_name

let html_debug_new_node_session ?kind ~pp_name node ~f =
  (get_callbacks ()).html_debug_new_node_session_f ?kind ~pp_name node ~f


let proc_resolve_attributes proc_name = (get_callbacks ()).proc_resolve_attributes_f proc_name
