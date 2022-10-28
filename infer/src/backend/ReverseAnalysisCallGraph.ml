(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

let build () =
  let graph = CallGraph.(create default_initial_capacity) in
  Summary.OnDisk.iter_specs ~f:(fun summary ->
      let caller = Summary.get_proc_name summary in
      let callees = summary.callee_pnames in
      Procname.Set.iter
        (fun callee -> CallGraph.add_edge graph ~pname:callee ~successor_pname:caller)
        callees ) ;
  graph
