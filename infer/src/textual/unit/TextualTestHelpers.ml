(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Textual

let sourcefile = SourceFile.create "dummy.sil"

let parse_module text =
  match TextualParser.parse_string sourcefile text with
  | Ok m ->
      m
  | Error es ->
      List.iter es ~f:(fun e -> F.printf "%a@\n" (TextualParser.pp_error sourcefile) e) ;
      raise (Failure "Couldn't parse a module")


let parse_module_print_errors text =
  match TextualParser.parse_string sourcefile text with
  | Ok _ ->
      raise (Failure "Successfuly parsed a module while expected parsing to fail")
  | Error es ->
      List.iter es ~f:(fun e -> F.printf "%a" (TextualParser.pp_error sourcefile) e)
