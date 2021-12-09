(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Checker for when an Objective-C block is a parameter of a function or method, and it's executed
    in the method's body without being checked for null first. *)

open! IStd

val checker : IntraproceduralAnalysis.t -> unit
