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


(* aggregate all of the procedures in the file env by their declaring
   class. this lets us analyze each class individually *)
let aggregate_by_class procedures =
  let open IOption.Let_syntax in
  List.fold procedures ~init:(AggregatedSummaries.make_empty ()) ~f:(fun map_to_update procname ->
      (let* class_name = Procname.get_class_type_name procname in
       let* java_class_name = get_java_class_name class_name in
       let* summary = Ondemand.analyze_proc_name_no_caller procname in
       return (AggregatedSummaries.register_summary java_class_name summary map_to_update))
      |> Option.value ~default:map_to_update )


(* Given list of proc summaries belonging to a class, aggregate it and add issues to the log, if needed *)
let analyze_class tenv source_file issue_log (class_name, class_info) =
  let is_from_third_party =
    ThirdPartyAnnotationInfo.is_third_party_class_name
      (ThirdPartyAnnotationGlobalRepo.get_repo ())
      class_name
  in
  if is_from_third_party then (* Don't analyze third party classes *)
    issue_log
  else ClassLevelAnalysis.analyze_class tenv source_file class_name class_info issue_log


let analyze_file {InterproceduralAnalysis.procedures; file_exe_env; source_file} =
  let class_map = aggregate_by_class procedures in
  let tenv = Exe_env.load_java_global_tenv file_exe_env in
  let user_class_info = AggregatedSummaries.group_by_user_class class_map in
  List.fold user_class_info ~init:IssueLog.empty ~f:(analyze_class tenv source_file)
