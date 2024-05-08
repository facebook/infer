(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val report_json : unit -> unit

val report_taint : lineage_source:string -> lineage_sink:string -> unit
