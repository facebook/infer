(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Javalib_pack
open Javalib
open JCode
open JBasics

let get_method_loc m =
  match m.cm_implementation with
  | Native ->
      0
  | Java code -> (
      let jcode = Lazy.force code in
      match jcode.c_line_number_table with
      | None ->
          0
      | Some line_table ->
          let lines = List.map (fun (_, n) -> n) line_table in
          let start_line = List.fold_left min max_int lines in
          let end_line = List.fold_left max 0 lines in
          end_line - start_line + 1 )


let get_class_loc map i_or_c =
  match i_or_c with
  | JInterface _ ->
      ()
  | JClass cl ->
      let methods = get_concrete_methods i_or_c in
      let class_loc = MethodMap.fold (fun _ v acc -> get_method_loc v + acc) methods 0 in
      Hashtbl.add map (cn_name cl.c_name) class_loc
