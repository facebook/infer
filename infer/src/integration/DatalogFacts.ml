(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** To avoid errors in Soufflé, ensure there is a file for every fact type, even if empty. *)
let create_fact_files facts_dir =
  Utils.create_dir facts_dir ;
  Fact.iter_fact_types (fun fact_typ ->
      ignore (Utils.create_outfile (F.asprintf "%s/%s.facts" facts_dir fact_typ)) )


let get_fact_type (jsonbug : Jsonbug_t.jsonbug) =
  if String.equal jsonbug.bug_type "DATALOG_FACT" then
    Some (List.hd_exn (String.split ~on:' ' jsonbug.qualifier))
  else None


let one_fact_to_report fmt (jsonbug : Jsonbug_t.jsonbug) =
  if String.equal jsonbug.bug_type "DATALOG_FACT" then
    let fields = List.drop (String.split ~on:' ' (String.drop_suffix jsonbug.qualifier 1)) 1 in
    let pp_sep fmt () = F.fprintf fmt "\t" in
    let pp_field fmt field = F.fprintf fmt "%s" field in
    let pp_fields = F.pp_print_list ~pp_sep pp_field in
    F.fprintf fmt "%a@." pp_fields fields


let create_from_json ~datalog_dir ~report_json =
  let report = Atdgen_runtime.Util.Json.from_file Jsonbug_j.read_report report_json in
  create_fact_files datalog_dir ;
  List.iter report ~f:(fun jsonbug ->
      match get_fact_type jsonbug with
      | Some fact_typ ->
          Utils.with_file_out (F.asprintf "%s/%s.facts" datalog_dir fact_typ) ~append:true
            ~f:(fun fact_file_out ->
              let report_fmt = F.formatter_of_out_channel fact_file_out in
              one_fact_to_report report_fmt jsonbug )
      | None ->
          () )
