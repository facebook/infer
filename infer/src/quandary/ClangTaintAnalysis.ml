(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module F = Format
module L = Logging

include
  TaintAnalysis.Make(struct
    module Trace = ClangTrace
    module AccessTree = AccessTree.Make(Trace)

    let to_summary_access_tree tree = QuandarySummary.AccessTree.Clang tree

    let of_summary_access_tree = function
      | QuandarySummary.AccessTree.Clang tree -> tree
      | _ -> assert false

    let handle_unknown_call _ ret_typ_opt actuals _ = match ret_typ_opt, actuals with
      | Some _, _ ->
          (* propagate taint from actuals to return value *)
          [TaintSpec.Propagate_to_return]
      | None, [] -> []
      | None, _ ->
          (* no return value; propagate taint from actuals to receiver *)
          [TaintSpec.Propagate_to_receiver]

    let is_taintable_type _ = true
  end)
