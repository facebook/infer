(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Core
open! Javalib_pack
open Javalib
open JCode
open JBasics

let get_method_loc m =
  match m with
  | ConcreteMethod m -> (
    match m.cm_implementation with
    | Native ->
        0
    | Java code -> (
        let jcode = Lazy.force code in
        match jcode.c_line_number_table with
        | None ->
            0
        | Some line_table ->
            let lines = List.map ~f:(fun (_, n) -> n) line_table in
            let start_line = List.fold lines ~init:Int.max_value ~f:min in
            let end_line = List.fold lines ~init:0 ~f:max in
            end_line - start_line + 1 ) )
  | AbstractMethod _ ->
      0


let get_class_loc cl = MethodMap.fold (fun _ m acc -> get_method_loc m + acc) cl.c_methods 0
