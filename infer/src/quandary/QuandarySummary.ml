(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** summary type for Quandary taint analysis *)

open! IStd

module F = Format
module L = Logging

module Java = AccessTree.Make(JavaTrace)
module Cpp = AccessTree.Make(CppTrace)

module AccessTree = struct
  type t =
    | Java of Java.t
    | Cpp of Cpp.t

  let pp fmt = function
    | Java access_tree -> Java.pp fmt access_tree
    | Cpp access_tree -> Cpp.pp fmt access_tree
end

type t = AccessTree.t

let pp = AccessTree.pp
