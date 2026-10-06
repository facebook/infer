(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseModelsImport

val alloc_common :
  null_case:bool -> initialize:bool -> desc:string -> Attribute.allocator -> Exp.t option -> model
(** allocation that may also return null when [null_case] is true *)

val matchers : matcher list
