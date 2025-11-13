(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseBasicInterface

type iteration_info = {timestamp: Timestamp.t; path_stamp: Formula.path_stamp}
[@@deriving compare, equal]

type loop_info = iteration_info list [@@deriving compare, equal]

type t [@@deriving compare, equal]

type id = Procdesc.Node.id

val empty : t

val mem : id -> t -> bool

val has_previous_iteration_same_path_stamp : id -> t -> bool

val is_current_iteration_empty_path_stamp : id -> t -> bool

val push_loop_info : id -> Timestamp.t -> Formula.t -> t -> t

val init_loop_info : id -> t -> t

val get_iteration_index : id -> t -> int

val pp : F.formatter -> t -> unit
