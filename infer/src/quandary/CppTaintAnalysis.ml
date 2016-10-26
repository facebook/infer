(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module F = Format
module L = Logging

include
  TaintAnalysis.Make(struct
    module Trace = CppTrace

    let to_summary_trace trace = QuandarySummary.Cpp trace

    let of_summary_trace = function
      | QuandarySummary.Cpp trace -> trace
      | _ -> assert false

    let handle_unknown_call _ _ _ =
      []
  end)
