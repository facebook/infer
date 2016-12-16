(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd


(** summary type for Quandary taint analysis *)

module F = Format

module Java : module type of (AccessTree.Make(JavaTrace))
module Cpp : module type of (AccessTree.Make(CppTrace))

module AccessTree : sig
  type t =
    | Java of Java.t
    | Cpp of Cpp.t
end

type t = AccessTree.t

val pp : F.formatter -> t -> unit
