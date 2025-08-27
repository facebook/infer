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
  .source_language = "Rust"

  define example::foo() : void {
    local var_0: int, var_1: int
    #node_0:
        store &var_1 <- 42
        store &var_0 <- null
        n0 = load &var_0
        ret n0

  }
|}]


let%expect_test "call_and_temp" =
  test "call_and_temp.ullbc" ;
  [%expect
    {|
  .source_language = "Rust"

  define call_and_temp::calls_and_temp() : int {
    local var_0: int, var_1: int

    #node_0:
        n0 = callee(3)
        store &var_1 <- n0
        n1 = load &var_1
        ret n1

  }

  define call_and_temp::callee(n: int) : int {
    local var_0: int, var_1: int

    #node_0:
        n0 = load &n
        n1 = __sil_mult_int(n0, 2)
        ret n1

  }

  define call_and_temp::main() : void {
    local var_0: void, var_1: int

    #node_0:
        n0 = calls_and_temp()
        store &var_1 <- n0
        store &var_0 <- null
        ret var_0
        
  }
  |}]


let%expect_test "int_comparison" =
  test "int_comparison.ullbc" ;
  [%expect
    {|
  .source_language = "Rust"

  define int_comparison::swi_cmp(x: int, y: int) : int {
    local var_3: bool, var_4: int, var_5: int
    #node_0:
        n0 = load &x
        store &var_4 <- n0
        n1 = load &y
        store &var_5 <- n1
        n2 = __sil_gt(var_4, var_5)
        if n2 then jmp node_1 else jmp node_2

    #node_1:
        ret var_4

    #node_2:
        ret var_5

  }

  define int_comparison::main() : void {
    local x: int, y: int, var_0: void, var_1: int, var_2: int
    #node_0:
        store &x <- 0
        store &y <- 1
        n0 = load &x
        store &var_1 <- n0
        n1 = load &y
        store &var_2 <- n1
        n2 = int_comparison::swi_cmp(var_1, var_2)
        store &var_0 <- null
        ret var_0
  }
  
  |}]


let%expect_test "nested" =
  test "nested.ullbc" ;
  [%expect
    {|
  .source_language = "Rust"

  define nested::swi_nested(a: int, b: int) : int {
    local var_3: bool, var_4: int, var_5: bool, var_6: int

    #node_0:
        n0 = load &a
        store &var_4 <- n0
        n1 = __sil_ge(var_4, 0)
        if n1 then jmp node_1 else jmp node_4

    #node_1:
        n2 = load &b
        store &var_6 <- n2
        n3 = __sil_eq(var_6, 0)
        if n3 then jmp node_2 else jmp node_3

    #node_2:
        ret 1

    #node_3:
        ret 2

    #node_4:
        ret 3

  }

  define nested::main() : void {
    local var_0: void, var_1: int, var_2: int, var_3: int, var_4: int

    #node_0:
        store &var_1 <- 0
        store &var_2 <- 1
        n0 = load &var_1
        store &var_3 <- n0
        n1 = load &var_2
        store &var_4 <- n1
        n2 = nested::swi_nested(var_3, var_4)
        store &var_0 <- null
        ret var_0

  }
  
  |}]
