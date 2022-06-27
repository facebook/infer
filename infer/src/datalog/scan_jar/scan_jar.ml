(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Javalib_pack
open Printf
open Loc

let run input_jar print_class =
  let class_loc_map = Hashtbl.create 10 in
  Javalib.iter (get_class_loc class_loc_map) input_jar ;
  let total_loc = Hashtbl.fold (fun _ v acc -> v + acc) class_loc_map 0 in
  if print_class then Hashtbl.iter (fun k v -> printf "Class %s LOC: %d\n" k v) class_loc_map ;
  printf "%s loc_estimate: %d n_classes: %d\n" (Filename.basename input_jar) total_loc
    (Hashtbl.length class_loc_map)


let () =
  let input_jar = ref "" in
  let output_name = ref None in
  let print_class = ref false in
  let spec_list =
    [ ("-o", Arg.String (fun name -> output_name := Some name), "Choose output name")
    ; ("--print-class", Arg.Set print_class, "Print LOC of all classes") ]
  in
  Arg.parse spec_list (fun filename -> input_jar := filename) "scan_entries <jar file>" ;
  let () = JBasics.set_permissive true in
  run !input_jar !print_class
