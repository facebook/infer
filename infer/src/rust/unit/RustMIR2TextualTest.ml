(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let program_path = "./programs"

let test json_file =
  try
    let file_path = Filename.concat program_path json_file in
    let file_path = Filename.concat (Unix.getcwd ()) file_path in
    let json = Yojson.Basic.from_file file_path in
    match Charon.UllbcOfJson.crate_of_json json with
    | Ok crate ->
        let textual = RustFrontend.RustMir2Textual.mk_module crate json_file in
        Textual.Module.pp F.std_formatter textual
    | Error err ->
        F.printf "Test failed: %s" err
  with e -> F.printf "%s" (Exn.to_string e)


let%expect_test "example" =
  test "example.ullbc" ;
  [%expect
  {|
  .source_language = "rust"

  define foo() : void {
    local var_0: void, var_1: int
    #node_0:
      store &var_1 <- 42
      store &var_0 <- null
      n0 = load &var_0
      ret n0
  }
|}]
