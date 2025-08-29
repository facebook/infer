(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open IStd
open RustMir2TextualTest

(* Tests for const *)
let%expect_test "literal_float" =
  test "./operands/const/literal_float.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define literal_float::main() : void {
      local var_0: void, var_1: float
      #node_0:
          store &var_1 <- 3.14:float
          store &var_0 <- null:void
          n0:void = load &var_0
          ret n0

    }

    |}]


let%expect_test "literal_int" =
  test "./operands/const/literal_int.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define literal_int::main() : void {
      local var_0: void, var_1: int
      #node_0:
          store &var_1 <- 42:int
          store &var_0 <- null:void
          n0:void = load &var_0
          ret n0
          
    }

    |}]


(* TODO: Enable literal_str test when the strings are supported *)

(*
let%expect_test "literal_str" =
  test "./operands/const/literal_str.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define literal_str::main() : void {
      local var_0: void, var_1: *String
      #node_0:
          store &var_1 <- "hello":
          store &var_0 <- null:void
          n0:void = load &var_0
          ret n0
        
    }

    |}]
*)

let%expect_test "literal_unit" =
  test "./operands/const/literal_unit.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define literal_unit::main() : void {
      local var_0: void, var_1: void
      #node_0:
          store &var_0 <- null:void
          n0:void = load &var_0
          ret n0
        
    }

    |}]


(* Tests for copy *)
let%expect_test "basic_copy" =
  test "./operands/copy/basic_copy.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define basic_copy::main() : void {
      local var_0: void, x: int, y: int
      #node_0:
          store &x <- 42:int
          n0:int = load &x
          store &y <- n0:int
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1
        
    }

    |}]


let%expect_test "copy_from_return" =
  test "./operands/copy/copy_from_return.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define copy_from_return::make() : int {
      local var_0: int
      #node_0:
          store &var_0 <- 5:int
          n0:int = load &var_0
          ret n0

    }

    define copy_from_return::main() : void {
      local var_0: void, x: int, y: int
      #node_0:
          n0 = make()
          store &x <- n0:int
          jmp node_1

      #node_1:
          n1:int = load &x
          store &y <- n1:int
          store &var_0 <- null:void
          n2:void = load &var_0
          ret n2

    }

    |}]


let%expect_test "copy_in_exp" =
  test "./operands/copy/copy_in_exp.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define copy_in_exp::main() : void {
      local var_0: void, a: int, b: int, c: int, var_4: int, var_5: int, var_6: int
      #node_0:
          store &a <- 10:int
          store &b <- 20:int
          n0:int = load &a
          store &var_4 <- n0:int
          n1:int = load &b
          store &var_5 <- n1:int
          n2:int = load &var_4
          n3:int = load &var_5
          store &var_6 <- __sil_plusa_int(n2, n3)
          n4:int = load &var_6
          store &c <- n4:int
          store &var_0 <- null:void
          n5:void = load &var_0
          ret n5

    }

    |}]


let%expect_test "nested_copy" =
  test "./operands/copy/nested_copy.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define nested_copy::main() : void {
      local var_0: void, x: int, y: int
      #node_0:
          store &x <- 100:int
          n0:int = load &x    
          store &y <- n0:int
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

    }

    |}]


(* Tests for move - TODO: Enable these when the strings are supported *)

(*
let%expect_test "basic_move" =
  ignore(test "./operands/move/basic_move.ullbc") ;
  [%expect
    {|
    .source_language = "Rust"

    define basic_move::main() : void {
      local var_0: void, var_1: *String, var_2: *String
      #node_0:
          store &var_1 <- "hello":*String
          n0: *String = load &var_1
          store &var_2 <- n0
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

    }

    |}]


let%expect_test "move_chain" =
  ignore(test "./operands/move/move_chain.ullbc") ;
  [%expect
    {|
    .source_language = "Rust"

    define move_chain::main() : void {
      local var_0: void, var_1: *String, var_2: *String, var_3: *String
      #node_0:
          store &var_1 <- "hello":*String
          n0: *String = load &var_1
          store &var_2 <- n0
          n1: *String = load &var_2
          store &var_3 <- n1
          n2: *String = load &var_3
          n3: void = __sil_free(n2)
          store &var_0 <- null:void
          n4:void = load &var_0
          ret n4
        
    }

    |}]


let%expect_test "move_from_return" =
  test "./operands/move/move_from_return.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define move_from_return::make_string() : *String {
      local var_0: *String
      #node_0:
          store &var_0 <- "hello":*String
          n0: *String = load &var_0
          ret n0

    }

    define move_from_return::main() : void {
      local var_0: void, var_1: *String
      #node_0:
          n0: *String = make_string()
          store &var_1 <- n0

          n1: *String = load &var_1
          n2: void = __sil_free(n1)

          store &var_0 <- null:void
          n3:void = load &var_0
          ret n3

    }

    |}]
*)
