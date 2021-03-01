(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseOperations.Import

let mk_objc_self_pvar proc_desc =
  let proc_name = Procdesc.get_proc_name proc_desc in
  Pvar.mk Mangled.self proc_name


let mk_objc_method_nil_summary_aux proc_desc astate =
  let location = Procdesc.get_loc proc_desc in
  let self = mk_objc_self_pvar proc_desc in
  let* astate, self_value = PulseOperations.eval_deref location (Lvar self) astate in
  let astate = PulseArithmetic.prune_eq_zero (fst self_value) astate in
  let ret_var = Procdesc.get_ret_var proc_desc in
  let ret_addr = AbstractValue.mk_fresh () in
  let ret_value = (ret_addr, []) in
  let* astate, ret_var_addr_hist = PulseOperations.eval Write location (Lvar ret_var) astate in
  let* astate = PulseOperations.write_deref location ~ref:ret_var_addr_hist ~obj:ret_value astate in
  let astate = PulseArithmetic.and_eq_int ret_addr IntLit.zero astate in
  let+ astate =
    PulseOperations.invalidate location (ConstantDereference IntLit.zero) ret_value astate
  in
  [astate]


let mk_objc_method_nil_summary ({InterproceduralAnalysis.proc_desc} as analysis_data) initial =
  let proc_name = Procdesc.get_proc_name proc_desc in
  match (initial, proc_name) with
  | ContinueProgram astate, Procname.ObjC_Cpp {kind= ObjCInstanceMethod} ->
      let result = mk_objc_method_nil_summary_aux proc_desc astate in
      Some (PulseReport.report_list_result analysis_data result)
  | ContinueProgram _, _
  | ExitProgram _, _
  | AbortProgram _, _
  | LatentAbortProgram _, _
  | ISLLatentMemoryError _, _ ->
      None


let append_objc_self_positive ({InterproceduralAnalysis.proc_desc} as analysis_data) astate =
  let location = Procdesc.get_loc proc_desc in
  let self = mk_objc_self_pvar proc_desc in
  match astate with
  | ContinueProgram astate ->
      let result =
        let+ astate, value = PulseOperations.eval_deref location (Lvar self) astate in
        let astate = PulseArithmetic.prune_positive (fst value) astate in
        [astate]
      in
      PulseReport.report_list_result analysis_data result
  | ExitProgram _ | AbortProgram _ | LatentAbortProgram _ | ISLLatentMemoryError _ ->
      [astate]


let update_objc_method_posts analysis_data ~initial_astate ~posts =
  let nil_summary = mk_objc_method_nil_summary analysis_data initial_astate in
  match nil_summary with
  | None ->
      posts
  | Some nil_summary ->
      let posts = List.concat_map ~f:(append_objc_self_positive analysis_data) posts in
      nil_summary @ posts
