(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let get_java_class_name = function
  | Typ.JavaClass java_class_name ->
      Some java_class_name
  | _ ->
      None


(* Fetch the class and summaries for each procedure *)
let get_summaries
    ({procedures; analyze_file_dependency} : NullsafeSummary.t InterproceduralAnalysis.file_t) =
  let open IOption.Let_syntax in
  List.filter_map procedures ~f:(fun procname ->
      let* class_name = Procname.get_class_type_name procname in
      let* java_class_name = get_java_class_name class_name in
      let* _proc_desc, summary = analyze_file_dependency procname in
      return (java_class_name, summary) )


(* Analyze the class and all its nested children recursively *)
let rec analyze_class_and_nested tenv source_file issue_log class_info =
  (* Analyze the class itself *)
  let updated_log = ClassLevelAnalysis.analyze_class tenv source_file class_info issue_log in
  (* Analyze its nested children *)
  AggregatedSummaries.ClassInfo.get_nested_classes_info class_info
  |> List.fold ~init:updated_log ~f:(analyze_class_and_nested tenv source_file)


(* Given aggregated information about the top-level class, analyze it and its nested children *)
let analyze_top_level_class tenv source_file issue_log top_level_class_info =
  let is_from_third_party =
    ThirdPartyAnnotationInfo.is_third_party_class_name
      (ThirdPartyAnnotationGlobalRepo.get_repo ())
      (AggregatedSummaries.ClassInfo.get_class_name top_level_class_info)
  in
  if is_from_third_party then (* Don't analyze third party classes *)
    issue_log
  else analyze_class_and_nested tenv source_file issue_log top_level_class_info


let analyze_file ({InterproceduralAnalysis.file_exe_env; source_file} as analysis_data) =
  let all_summaries = get_summaries analysis_data in
  let tenv = Exe_env.load_java_global_tenv file_exe_env in
  let top_level_classes = AggregatedSummaries.aggregate all_summaries in
  List.fold top_level_classes ~init:IssueLog.empty ~f:(analyze_top_level_class tenv source_file)
