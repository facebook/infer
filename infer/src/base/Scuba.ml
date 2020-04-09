(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module SMap = Map.Make (String)

type table = InferEvents

type sample =
  { int_section: int SMap.t  (** All integer type fields and their values *)
  ; normal_section: string SMap.t
        (** All string (normal in Scuba terminology) type fields and their values *)
  ; tagset_section: string list SMap.t
        (** All sets of strings (tagsets in Scuba terminology) type fields and their values *) }

let new_sample ~time =
  let time = match time with Some time -> time | None -> int_of_float (Unix.time ()) in
  { (* time is a single mandatory field in scuba. without it,
       scuba disregards all samples *)
    int_section= SMap.singleton "time" time
  ; normal_section= SMap.empty
  ; tagset_section= SMap.empty }


let add_int ~name ~value sample =
  let int_section = SMap.set sample.int_section ~key:name ~data:value in
  {sample with int_section}


let add_normal ~name ~value sample =
  let normal_section = SMap.set sample.normal_section ~key:name ~data:value in
  {sample with normal_section}


let add_tagset ~name ~value sample =
  let tagset_section = SMap.set sample.tagset_section ~key:name ~data:value in
  {sample with tagset_section}


let sample_to_json sample =
  let map_to_assoc value_to_json key_value_map =
    let pairs = SMap.to_alist key_value_map in
    let assocs = List.map pairs ~f:(fun (name, data) -> (name, value_to_json data)) in
    `Assoc assocs
  in
  let ints_to_assoc = map_to_assoc (fun data -> `Int data) in
  let normals_to_assoc = map_to_assoc (fun data -> `String data) in
  let tags_to_assoc = map_to_assoc (fun data -> `List (List.map data ~f:(fun d -> `String d))) in
  `Assoc
    [ ("int", ints_to_assoc sample.int_section)
    ; ("normal", normals_to_assoc sample.normal_section)
    ; ("tags", tags_to_assoc sample.tagset_section) ]


let sample_to_json_string sample = sample |> sample_to_json |> Yojson.Basic.to_string

let table_to_scribe_category = function InferEvents -> Scribe.InferEvents

let log table samples =
  let category = table_to_scribe_category table in
  Scribe.log category (List.map samples ~f:sample_to_json_string)


let log = if Config.scuba_logging then log else fun _ _ -> ()
