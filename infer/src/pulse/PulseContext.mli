(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val proc_desc : unit -> Procdesc.t option

val tenv : unit -> Tenv.t option [@@warning "-unused-value-declaration"]

val tenv_exn : unit -> Tenv.t

val set_tenv_global_for_testing : Tenv.t -> unit [@@warning "-unused-value-declaration"]
