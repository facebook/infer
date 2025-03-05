(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module SMap = IString.Map

type sample =
  { int_section: int SMap.t  (** All integer type fields and their values *)
  ; normal_section: string SMap.t  (** All string type fields and their values *)
  ; tagset_section: string list SMap.t  (** All sets of strings type fields and their values *) }

let new_sample ~time =
  let time = match time with Some time -> time | None -> int_of_float (Caml_unix.time ()) in
  { (* time is a single mandatory field. *)
    int_section= SMap.singleton "time" time
  ; normal_section= SMap.empty
  ; tagset_section= SMap.empty }


let add_int ~name ~value sample =
  let int_section = SMap.add name value sample.int_section in
  {sample with int_section}


let add_normal ~name ~value sample =
  let normal_section = SMap.add name value sample.normal_section in
  {sample with normal_section}


let sample_to_json sample =
  let map_to_assoc value_to_json key_value_map =
    let pairs = SMap.bindings key_value_map in
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
