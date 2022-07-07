(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Core
open! Javalib_pack
open Javalib
open JBasics
open Printf
open Loc
open Reflect

let run input_jar =
  let class_loc_map = Hashtbl.create (module String) in
  let class_reflect_map = Hashtbl.create (module String) in
  Javalib.iter
    (fun i_or_c ->
      match i_or_c with
      | JClass cl ->
          (* Get lines of code *)
          Hashtbl.add_exn class_loc_map ~key:(cn_name cl.c_name) ~data:(get_class_loc cl) ;
          (* Get reflection usages *)
          let calls = get_class_refl_calls cl in
          if not (List.is_empty calls) then
            Hashtbl.add_exn class_reflect_map ~key:(cn_name cl.c_name) ~data:calls
      | _ ->
          () )
    input_jar ;
  let total_loc = Hashtbl.fold ~init:0 ~f:(fun ~key:_ ~data acc -> data + acc) class_loc_map in
  printf "%s loc_estimate: %d n_classes: %d\n" (Filename.basename input_jar) total_loc
    (Hashtbl.length class_loc_map) ;
  if Hashtbl.length class_reflect_map > 0 then printf "-----\n" ;
  Hashtbl.iteri
    ~f:(fun ~key:cn ~data:m_list ->
      printf "Reflection usage(s) found in class %s:\n" cn ;
      List.iter ~f:(fun (rm, cm) -> printf "'%s' in caller method '%s'\n" rm cm) m_list )
    class_reflect_map


let () =
  let input_jar = ref "" in
  let output_name = ref None in
  let spec_list =
    [("-o", Arg.String (fun name -> output_name := Some name), "Choose output name")]
  in
  Arg.parse spec_list (fun filename -> input_jar := filename) "scan_entries <jar file>" ;
  let () = JBasics.set_permissive true in
  run !input_jar
