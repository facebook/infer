(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let program_path = "./programs"

let test json_filename =
  try
    let file_path = Filename.concat program_path json_filename in
    let json = Yojson.Basic.from_file file_path in
    match Charon.UllbcOfJson.crate_of_json json with
    | Ok _crate ->
        F.printf "Not yet implemented"
    | Error err ->
        F.printf "Test failed: %s" err
  with e -> F.printf "%s" (Exn.to_string e)

(* An example test *)
(*
let%expect_test "example" =
  test "./example.ullbc" ;
  [%expect
    {|
  .source_language = "Rust"

  define example::foo() : void {
    local var_0: void, x_1: int
    #node_0:
        store &x_1 <- 42:int
        store &var_0 <- null:void
        store &var_0 <- null:void
        n0:void = load &var_0
        ret n0

  }

  define example::main() : void {
    local var_0: void, var_1: void
    #node_0:
        n0 = example::foo()
        store &var_1 <- n0:void
        jmp node_1

    #node_1:
        store &var_0 <- null:void
        store &var_0 <- null:void
        n1:void = load &var_0
        ret n1

    #node_2:
        unreachable

  }
|}]
*)
