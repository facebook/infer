(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val write : xml_path:string -> json_path:string -> unit
(** read the JSON report at [json_path] and translates it to a PMD-style XML report in [xml_path] *)
