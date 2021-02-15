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


let mk_interprocedural_t ~f_analyze_dep ~get_payload exe_env summary
    ?(tenv = Exe_env.get_proc_tenv exe_env (Summary.get_proc_name summary)) () =
  let analyze_dependency proc_name =
    let summary = Ondemand.analyze_proc_name exe_env ~caller_summary:summary proc_name in
    Option.bind summary ~f:(fun {Summary.payloads; proc_desc; _} ->
        f_analyze_dep proc_desc (get_payload payloads) )
  in
  let stats = ref summary.Summary.stats in
  let update_stats ?add_symops ?failure_kind () =
    stats := Summary.Stats.update ?add_symops ?failure_kind !stats
  in
  ( { InterproceduralAnalysis.proc_desc= Summary.get_proc_desc summary
    ; tenv
    ; err_log= Summary.get_err_log summary
    ; exe_env
    ; analyze_dependency
    ; update_stats }
  , stats )


let mk_interprocedural_field_t payload_field exe_env summary =
  mk_interprocedural_t
    ~f_analyze_dep:(fun pdesc payload_opt ->
      Option.map payload_opt ~f:(fun payload -> (pdesc, payload)) )
    ~get_payload:(Field.get payload_field) exe_env summary


let interprocedural ~f_analyze_dep ~get_payload ~set_payload checker {Callbacks.summary; exe_env} =
  let analysis_data, stats_ref =
    mk_interprocedural_t ~f_analyze_dep ~get_payload exe_env summary ()
  in
  let result = checker analysis_data in
  {summary with payloads= set_payload summary.payloads result; stats= !stats_ref}


let interprocedural_with_field payload_field checker {Callbacks.summary; exe_env} =
  let analysis_data, stats_ref = mk_interprocedural_field_t payload_field exe_env summary () in
  let result = checker analysis_data in
  {summary with payloads= Field.fset payload_field summary.payloads result; stats= !stats_ref}


let interprocedural_file payload_field checker {Callbacks.procedures; exe_env; source_file} =
  let analyze_file_dependency proc_name =
    let summary = Ondemand.analyze_proc_name_no_caller exe_env proc_name in
    Option.bind summary ~f:(fun {Summary.payloads; proc_desc; _} ->
        Field.get payload_field payloads |> Option.map ~f:(fun payload -> (proc_desc, payload)) )
  in
  checker
    {InterproceduralAnalysis.procedures; source_file; file_exe_env= exe_env; analyze_file_dependency}


let to_intraprocedural_t {Callbacks.summary; exe_env} =
  { IntraproceduralAnalysis.proc_desc= Summary.get_proc_desc summary
  ; tenv= Exe_env.get_proc_tenv exe_env (Summary.get_proc_name summary)
  ; err_log= Summary.get_err_log summary }


let intraprocedural checker ({Callbacks.summary} as callbacks) =
  checker (to_intraprocedural_t callbacks) ;
  summary


let intraprocedural_with_field_dependency payload_field checker ({Callbacks.summary} as callbacks) =
  checker (to_intraprocedural_t callbacks) (Field.get payload_field summary.payloads) ;
  summary


let intraprocedural_with_field payload_field checker ({Callbacks.summary} as callbacks) =
  let result = checker (to_intraprocedural_t callbacks) in
  {summary with payloads= Field.fset payload_field summary.payloads result}
