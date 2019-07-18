(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

let register_summary graph summary =
  let caller_pname = Summary.get_proc_name summary in
  let callee_pnames = summary.Summary.callee_pnames in
  Typ.Procname.Set.iter
    (fun callee_pname -> CallGraph.add_edge graph ~pname:callee_pname ~successor_pname:caller_pname)
    callee_pnames


let build graph = SpecsFiles.iter ~f:(register_summary graph)
