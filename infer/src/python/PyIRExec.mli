(*
 * Copyright (c) Facebook, Inc. and its affiliates.[
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

val run : PyIR.Module.t -> unit [@@warning "-unused-value-declaration"]

val run_files : PyIR.Module.t list -> unit
