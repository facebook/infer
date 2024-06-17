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
  let procnames = Procedures.get_all ~filter () in
  let get_summary procname =
    Summary.OnDisk.get ~lazy_payloads:true (AnalysisRequest.one Pulse) procname
    |> Option.bind ~f:(fun {Summary.payloads= {pulse}} -> ILazy.force_option pulse)
  in
  PulseSummary.get_missed_captures ~get_summary procnames


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


let output type_name_set =
  let channel =
    ResultsDirEntryName.get_path ~results_dir:Config.results_dir ReactiveCaptureMissingTypes
    |> Out_channel.create
  in
  let fmt = F.formatter_of_out_channel channel in
  let _ =
    Typ.Name.Set.fold
      (fun typ_name seen ->
        let normalized_type_name = normalize_type_name typ_name in
        if String.Set.mem seen normalized_type_name then seen
        else (
          F.fprintf fmt "%s\n" normalized_type_name ;
          String.Set.add seen normalized_type_name ) )
      type_name_set String.Set.empty
  in
  Out_channel.close_no_err channel


let store_missed_captures ~source_files_filter () =
  let type_name_set = get_missed_captures source_files_filter in
  output type_name_set
