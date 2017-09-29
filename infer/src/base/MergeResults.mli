(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

val merge_buck_flavors_results : string -> unit
(** Merge the results from sub-invocations of infer inside buck-out/. Takes as argument the infer_deps file. *)
