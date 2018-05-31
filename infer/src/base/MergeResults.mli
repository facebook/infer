(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val merge_buck_flavors_results : string -> unit
(** Merge the results from sub-invocations of infer inside buck-out/. Takes as argument the
    infer_deps file. *)
