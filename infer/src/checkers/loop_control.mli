(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Control

val get_loop_control_maps : Procdesc.Node.t list Procdesc.NodeMap.t -> loop_control_maps
(** Get a pair of maps (exit_map, loop_head_to_guard_map) where exit_map : exit_node -> loop_head
    set (i.e. target of the back edges) loop_head_to_guard_map : loop_head -> guard_nodes and
    guard_nodes contains the nodes that may affect the looping behavior, i.e. occur in the guard of
    the loop conditional. *)
