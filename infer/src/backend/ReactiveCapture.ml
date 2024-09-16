(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let get_missed_captures source_files_filter =
  let filter sourcefile _procname =
    (Lazy.force Filtering.source_files_filter) sourcefile && source_files_filter sourcefile
  in
  let proc_names = Procedures.get_all ~filter () in
  let get_summary proc_name =
    Summary.OnDisk.get ~lazy_payloads:true (AnalysisRequest.one Pulse) proc_name
    |> Option.bind ~f:(fun {Summary.payloads= {pulse}} -> ILazy.force_option pulse)
  in
  let entry_nodes =
    List.map proc_names ~f:(fun proc_name -> {SpecializedProcname.proc_name; specialization= None})
  in
  PulseSpecializedCallGraph.get_missed_captures ~get_summary entry_nodes


let normalize_type_name (name : Typ.name) =
  match name with
  | HackClass name ->
      let base_name =
        if HackClassName.is_static_companion name then HackClassName.static_companion_origin name
        else name
      in
      F.asprintf "%a" HackClassName.pp base_name
  | _ ->
      F.asprintf "%a" Typ.Name.pp name


let output_json type_map =
  let out_file =
    ResultsDirEntryName.get_path ~results_dir:Config.results_dir ReactiveCaptureMissingTypes
  in
  let output_node_set node_set = SpecializedProcname.Set.sexp_of_t node_set |> Sexp.to_string in
  let type_list =
    Typ.Name.Map.fold
      (fun typ_name node_set acc ->
        let normalized_type_name = normalize_type_name typ_name in
        let node_set = output_node_set node_set in
        `Assoc [("type_name", `String normalized_type_name); ("node_set", `String node_set)] :: acc
        )
      type_map []
  in
  let json = `List (List.rev type_list) in
  Utils.with_file_out out_file ~f:(fun out_channel -> Yojson.to_channel out_channel json)


let store_missed_captures ~source_files_filter () =
  let type_map = get_missed_captures source_files_filter in
  output_json type_map
