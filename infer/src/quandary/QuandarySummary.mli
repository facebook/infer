(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** summary type for Quandary taint analysis *)

module F = Format

module Java : module type of AccessTree.Make (JavaTrace) (AccessTree.DefaultConfig)

module Clang : module type of AccessTree.Make (ClangTrace) (AccessTree.DefaultConfig)

module AccessTree : sig
  type t = Java of Java.t | Clang of Clang.t
end

type t = AccessTree.t

val pp : F.formatter -> t -> unit
