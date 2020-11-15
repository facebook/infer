(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let pp_annotation_point fmt
    Jsonbug_t.{id; kind; method_info; field_name; param_num; num_violations; dependent_point_ids} =
  let pp_id fmt = Format.fprintf fmt "id: %s@\n" id in
  let pp_kind fmt =
    let kind_str = match kind with `Field -> "Field" | `Method -> "Method" | `Param -> "Param" in
    Format.fprintf fmt "kind: %s@\n" kind_str
  in
  let pp_method_info fmt =
    Option.iter method_info ~f:(fun Jsonbug_t.{method_name; params; access_level} ->
        let access_level_str =
          match access_level with
          | `Public ->
              "Public"
          | `Private ->
              "Private"
          | `Protected ->
              "Protected"
          | `Default ->
              "Default"
        in
        Format.fprintf fmt "method_info:@\n  @[method_name: %s@\nparams: %s@\naccess_level: %s@]@\n"
          method_name (String.concat ~sep:", " params) access_level_str )
  in
  let pp_field_name fmt =
    Option.iter field_name ~f:(fun field_name -> Format.fprintf fmt "field_name: %s@\n" field_name)
  in
  let pp_param_num fmt =
    Option.iter param_num ~f:(fun param_num -> Format.fprintf fmt "param_num: %d@\n" param_num)
  in
  let pp_num_violations fmt = Format.fprintf fmt "num_violations: %d@\n" num_violations in
  let pp_dependent_point_ids fmt =
    Format.fprintf fmt "dependent_point_ids: [%s]@\n" (String.concat ~sep:", " dependent_point_ids)
  in
  let pp_point fmt =
    pp_id fmt ;
    pp_kind fmt ;
    pp_method_info fmt ;
    pp_field_name fmt ;
    pp_param_num fmt ;
    pp_num_violations fmt ;
    pp_dependent_point_ids fmt
  in
  Format.fprintf fmt "Annotation point:@\n  @[%t@]@\n" pp_point


let pp_annotation_graph = Pp.seq ~sep:"" pp_annotation_point
