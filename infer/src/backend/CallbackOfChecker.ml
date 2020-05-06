(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* make sure callbacks are set or the checkers will not be able to call into them (and get a nice
   crash) *)
let () =
  AnalysisCallbacks.set_callbacks
    { get_proc_desc_f= Ondemand.get_proc_desc
    ; html_debug_new_node_session_f= NodePrinter.with_session
    ; proc_resolve_attributes_f= Summary.OnDisk.proc_resolve_attributes }


let interprocedural payload_field checker {Callbacks.summary; exe_env} =
  let analyze_dependency proc_name =
    let summary = Ondemand.analyze_proc_name ~caller_summary:summary proc_name in
    Option.bind summary ~f:(fun {Summary.payloads; proc_desc; _} ->
        Field.get payload_field payloads |> Option.map ~f:(fun payload -> (proc_desc, payload)) )
  in
  let analyze_pdesc_dependency proc_desc =
    let summary = Ondemand.analyze_proc_desc ~caller_summary:summary proc_desc in
    Option.bind summary ~f:(fun {Summary.payloads; _} ->
        Field.get payload_field payloads |> Option.map ~f:(fun payload -> payload) )
  in
  let stats = ref summary.Summary.stats in
  let update_stats ?add_symops ?failure_kind () =
    stats := Summary.Stats.update ?add_symops ?failure_kind !stats
  in
  let result =
    checker
      { InterproceduralAnalysis.proc_desc= Summary.get_proc_desc summary
      ; tenv= Exe_env.get_tenv exe_env (Summary.get_proc_name summary)
      ; err_log= Summary.get_err_log summary
      ; exe_env
      ; analyze_dependency
      ; analyze_pdesc_dependency
      ; update_stats }
  in
  {summary with payloads= Field.fset payload_field summary.payloads result; stats= !stats}


let interprocedural_file payload_field checker {Callbacks.procedures; exe_env; source_file} =
  let analyze_file_dependency proc_name =
    let summary = Ondemand.analyze_proc_name_no_caller proc_name in
    Option.bind summary ~f:(fun {Summary.payloads; proc_desc; _} ->
        Field.get payload_field payloads |> Option.map ~f:(fun payload -> (proc_desc, payload)) )
  in
  checker
    {InterproceduralAnalysis.procedures; source_file; file_exe_env= exe_env; analyze_file_dependency}
