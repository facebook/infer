(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
open PulseBasicInterface

include
  PrettyPrintable.MonoMap with type key = Var.t and type value = AbstractValue.t * ValueHistory.t

(* need to shadow the declaration in [MonoMap] even though it is unused since [MapS.compare] has a
     different type *)
val compare : t -> t -> int [@@warning "-32"]

val pp : F.formatter -> t -> unit
