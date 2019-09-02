(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val merge_buck_flavors_results : string -> unit
(** Merge the results from sub-invocations of infer inside buck-out/. Takes as argument the
    infer_deps file. *)

val merge_buck_changed_functions : string -> unit
(** Merge the changed functions from sub-invocations of infer inside buck-out/. Takes as argument the
    infer_deps file. *)

val iter_infer_deps : string -> f:(infer_out_src:string -> unit) -> unit
