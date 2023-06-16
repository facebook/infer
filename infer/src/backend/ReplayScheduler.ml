(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let make call_graph sources =
  (* After the procedure-level tasks from [replay_call_graph] have been completed, run the
     file-level analyses. This is done by chaining the file scheduler after
     [replay_call_graph]. This is similar to how callgraph does it but the expectation here is that
     by then all procedure-level analyses required by those have been done already. *)
  ProcessPool.TaskGenerator.chain
    (CallGraphScheduler.bottom_up call_graph)
    (FileScheduler.make sources)
