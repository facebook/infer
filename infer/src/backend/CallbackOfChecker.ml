(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* make sure callbacks are set or the checkers will not be able to call into them (and get a nice
   crash) *)
let () = AnalysisCallbacks.set_callbacks {html_debug_new_node_session_f= NodePrinter.with_session}

let mk_interprocedural_t ~f_analyze_dep ~get_payload
    {Callbacks.exe_env; proc_desc; summary= {Summary.stats; proc_attrs; err_log} as caller_summary}
    ?(tenv = Exe_env.get_proc_tenv exe_env proc_attrs.proc_name) () =
  let analyze_dependency proc_name =
    let open IOption.Let_syntax in
    let* {Summary.payloads} = Ondemand.analyze_proc_name exe_env ~caller_summary proc_name in
    f_analyze_dep (get_payload payloads)
  in
  let stats = ref stats in
  let update_stats ?add_symops ?failure_kind () =
    stats := Summary.Stats.update ?add_symops ?failure_kind !stats
  in
  ( {InterproceduralAnalysis.proc_desc; tenv; err_log; exe_env; analyze_dependency; update_stats}
  , stats )


let mk_interprocedural_field_t payload_field =
  mk_interprocedural_t ~f_analyze_dep:Fn.id ~get_payload:(fun payloads ->
      Field.get payload_field payloads |> Lazy.force )


let interprocedural ~f_analyze_dep ~get_payload ~set_payload checker ({Callbacks.summary} as args) =
  let analysis_data, stats_ref = mk_interprocedural_t ~f_analyze_dep ~get_payload args () in
  let result = checker analysis_data |> Lazy.from_val in
  {summary with payloads= set_payload summary.payloads result; stats= !stats_ref}


let interprocedural_with_field payload_field checker ({Callbacks.summary} as args) =
  let analysis_data, stats_ref = mk_interprocedural_field_t payload_field args () in
  let result = checker analysis_data |> Lazy.from_val in
  {summary with payloads= Field.fset payload_field summary.payloads result; stats= !stats_ref}


let interprocedural_with_field_dependency ~dep_field payload_field checker
    ({Callbacks.summary} as callbacks) =
  let checker analysis_data =
    checker analysis_data (Field.get dep_field summary.payloads |> Lazy.force)
  in
  interprocedural_with_field payload_field checker callbacks


let interprocedural_file payload_field checker {Callbacks.procedures; exe_env; source_file} =
  let analyze_file_dependency proc_name =
    Ondemand.analyze_proc_name_no_caller exe_env proc_name
    |> Option.bind ~f:(fun {Summary.payloads; _} -> Field.get payload_field payloads |> Lazy.force)
  in
  checker
    {InterproceduralAnalysis.procedures; source_file; file_exe_env= exe_env; analyze_file_dependency}


let to_intraprocedural_t {Callbacks.summary= {proc_attrs; err_log}; exe_env; proc_desc} =
  { IntraproceduralAnalysis.proc_desc
  ; tenv= Exe_env.get_proc_tenv exe_env proc_attrs.proc_name
  ; err_log }


let intraprocedural checker ({Callbacks.summary} as callbacks) =
  checker (to_intraprocedural_t callbacks) ;
  summary


let intraprocedural_with_field_dependency payload_field checker ({Callbacks.summary} as callbacks) =
  checker (to_intraprocedural_t callbacks) (Field.get payload_field summary.payloads |> Lazy.force) ;
  summary


let intraprocedural_with_field payload_field checker ({Callbacks.summary} as callbacks) =
  let result = checker (to_intraprocedural_t callbacks) |> Lazy.from_val in
  {summary with payloads= Field.fset payload_field summary.payloads result}
