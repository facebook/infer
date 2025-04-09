(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type t

val gen_module_default_type : PyIR.Module.t -> t option

val gen_module_default_type_debug : PyIR.Module.t -> unit [@@warning "-unused-value-declaration"]
