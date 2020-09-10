(*
 * Copyright (c) 2014-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Clang_ast_j
open Yojson_utils

let data = {sl_file= Some "foo"; sl_line= Some 1; sl_column= None}

let basic_test pretty name =
  write_data_to_file ~pretty write_source_location name data ;
  let data3 = read_data_from_file read_source_location name in
  Unix.unlink name ;
  Utils.assert_equal (Printf.sprintf "test %s pretty=%b" name pretty) data data3


let files = List.map (( ^ ) "yojson_utils_test_tmpfile") [".gz"; ".value"; ".value.gz"; ".yjson"]

let test1 = List.iter (basic_test false) files

let test2 = List.iter (basic_test true) files
