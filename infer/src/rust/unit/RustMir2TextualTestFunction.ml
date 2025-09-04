(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open IStd
open RustMir2TextualTest

let%expect_test "caller_and_callee" =
  test "./function/caller_and_callee.ullbc" ;
  [%expect
    {|
  .source_language = "Rust"

  define caller_and_callee::caller() : int {
    local var_0: int, k_1: int
    #node_0:
        n0 = caller_and_callee::callee(3)
        store &k_1 <- n0:int
        jmp node_1

    #node_1:
        n1:int = load &k_1
        store &var_0 <- n1:int
        n2:int = load &var_0
        ret n2

    #node_2:
        unreachable

  }

  define caller_and_callee::callee(n_1: int) : int {
    local var_0: int, var_2: int, var_3: int
    #node_0:
        n0:int = load &n_1
        store &var_2 <- n0:int
        n1:int = load &var_2
        store &var_3 <- __sil_mult_int(n1, 2):int
        n2:int = load &var_3
        store &var_0 <- n2:int
        n3:int = load &var_0
        ret n3

  }

  define caller_and_callee::main() : void {
    local var_0: void, var_1: int
    #node_0:
        n0 = caller_and_callee::caller()
        store &var_1 <- n0:int
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


let%expect_test "calculate" =
  test "./function/calculate.ullbc" ;
  [%expect
    {|
  .source_language = "Rust"

  define calculate::calculate(a_1: int, b_2: int) : int {
    local var_0: int, result1_3: int, var_4: int, result2_5: int, var_6: int, var_7: int, var_8: int
    #node_0:
        n0:int = load &a_1          
        store &var_4 <- n0:int
        n1:int = load &var_4
        n2 = calculate::square(n1)
        store &result1_3 <- n2:int
        jmp node_1

    #node_1:
        n3:int = load &result1_3
        store &var_6 <- n3:int
        n4:int = load &var_6
        n5 = calculate::square(n4)
        store &result2_5 <- n5:int
        jmp node_3

    #node_2:
        unreachable

    #node_3:
        n6:int = load &result1_3
        store &var_7 <- n6:int
        n7:int = load &result2_5
        store &var_8 <- n7:int
        n8:int = load &var_7
        n9:int = load &var_8
        n10 = calculate::subtract(n8, n9)
        store &var_0 <- n10:int
        jmp node_4

    #node_4:
        n11:int = load &var_0
        ret n11

  }

  define calculate::square(x_1: int) : int {
    local var_0: int, var_2: int, var_3: int, var_4: int
    #node_0:
        n0:int = load &x_1
        store &var_2 <- n0:int
        n1:int = load &x_1
        store &var_3 <- n1:int
        n2:int = load &var_2
        n3:int = load &var_3
        store &var_4 <- __sil_mult_int(n2, n3):int
        n4:int = load &var_4
        store &var_0 <- n4:int
        n5:int = load &var_0
        ret n5

  }

  define calculate::subtract(x_1: int, y_2: int) : int {
    local var_0: int, var_3: int, var_4: int, var_5: int
    #node_0:
        n0:int = load &x_1
        store &var_3 <- n0:int
        n1:int = load &y_2
        store &var_4 <- n1:int
        n2:int = load &var_3
        n3:int = load &var_4
        store &var_5 <- __sil_minusa_int(n2, n3):int
        n4:int = load &var_5
        store &var_0 <- n4:int
        n5:int = load &var_0
        ret n5

  }

  define calculate::main() : void {
    local var_0: void, x_1: int, y_2: int, result_3: int, var_4: int, var_5: int
    #node_0:
        store &x_1 <- 0:int
        store &y_2 <- 1:int
        n0:int = load &x_1
        store &var_4 <- n0:int
        n1:int = load &y_2
        store &var_5 <- n1:int
        n2:int = load &var_4
        n3:int = load &var_5
        n4 = calculate::calculate(n2, n3)
        store &result_3 <- n4:int
        jmp node_1

    #node_1:
        store &var_0 <- null:void
        store &var_0 <- null:void
        n5:void = load &var_0
        ret n5

    #node_2:
        unreachable
        
  }

|}]
