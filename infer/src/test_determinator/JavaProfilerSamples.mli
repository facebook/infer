(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type labeled_profiler_sample = string * Typ.Procname.Set.t [@@deriving compare]

val equal_labeled_profiler_sample : labeled_profiler_sample -> labeled_profiler_sample -> bool

val from_json_string : string -> use_signature:bool -> labeled_profiler_sample list

val from_json_file : string -> use_signature:bool -> labeled_profiler_sample list
