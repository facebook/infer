(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseDomainInterface

val update_objc_method_posts :
     PulseSummary.t InterproceduralAnalysis.t
  -> initial_astate:ExecutionDomain.t
  -> posts:ExecutionDomain.t list
  -> ExecutionDomain.t list
(** For ObjC instance methods: adds path condition `self > 0` to given posts and appends additional
    nil summary. Does nothing to posts for other kinds of methods *)
