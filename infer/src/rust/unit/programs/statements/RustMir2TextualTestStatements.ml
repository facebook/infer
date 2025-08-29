(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open IStd
open RustMir2TextualTest

(* Tests for assign *)
let%expect_test "assign_binop" =
  test "./statements/assign/assign_binop.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define assign_binop::main() : void {
      local var_0: void, x: int, var_2: int
      #node_0:
          store &var_2 <- __sil_plusa_int(2, 3):int
          n0:int = load &var_2
          store &x <- n0:int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1
          
    }

  |}]


(* TODO: Enable assign_cast and assign_tuple tests when cast and tuple are supported*)

(*
let%expect_test "assign_cast" =
  test "./statements/assign/assign_cast.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define assign_cast::main() : void {
      local var_0: void, y: int, var_2: int
      #node_0:
          store &var_2 <- 5:int
          n0:int = load &var_2
          store &y <- n0:int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1
        
    }

  |}]


let%expect_test "assign_tuple" =
  test "./statements/assign/assign_tuple.ullbc" ;
  [%expect
    {|
   .source_language = "Rust"

    type tuple_int = { f0: int, f1: int }

    define assign_tuple::main() : void {
      local var_0: void, x: tuple_int
      #node_0:
          store &x.f0 <- 1:int
          store &x.f1 <- 2:int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1
        
    }

  |}]
*)

(* Tests for storage dead and storage live *)
let%expect_test "shadowing" =
  test "./statements/storage_dead_and_live/shadowing.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define shadowing::main() : void {
      local var_0: void, var_1: int, var_2: void, var_3: int
      #node_0:
          store &var_1 <- 1:int
          store &var_3 <- 2:int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n0:void = load &var_0
          ret n0

    }

  |}]


let%expect_test "shadowing2" =
  test "./statements/storage_dead_and_live/shadowing2.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define shadowing2::main() : void {
      local var_0: void, var_1: int, var_2: void, var_3: int, var_4: int, var_5: int, var_6: int, var_7: int, var_8: int, var_9: int, var_10: int, var_11: int, var_12: int
      #node_0:
          store &var_1 <- 10:int
          store &var_3 <- 20:int
          n0:int = load &var_3
          store &var_5 <- n0:int
          n1:int = load &var_5
          store &var_6 <- __sil_plusa_int(n1, 5):int
          n2:int = load &var_6
          store &var_4 <- n2:int
          n3:int = load &var_1
          store &var_8 <- n3:int
          n4:int = load &var_8
          store &var_9 <- __sil_plusa_int(n4, 100):int
          n5:int = load &var_9
          store &var_7 <- n5:int
          store &var_10 <- 30:int
          n6:int = load &var_10
          store &var_11 <- n6:int
          n7:int = load &var_11
          store &var_12 <- __sil_plusa_int(n7, 10):int
          n8:int = load &var_12
          store &var_10 <- n8:int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n9:void = load &var_0
          ret n9

    }
  |}]
