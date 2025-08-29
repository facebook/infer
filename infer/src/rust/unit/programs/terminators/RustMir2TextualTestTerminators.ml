(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open IStd
open RustMir2TextualTest

(* Tests for call *)
let%expect_test "basic_call" =
  test "./terminators/call/basic_call.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define basic_call::foo() : void {
      local var_0: void
      #node_0:
          n0:void = bar()
          store &var_0 <- n0
          jmp node_2

      #node_2:
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

    }

    define basic_call::bar() : void {
      local var_0: void
      #node_0:
          store &var_0 <- null
          n0:void = load &var_0
          ret n0

    }

    define basic_call::main() : void {
      local var_0: void
      #node_0:
          n0:void = foo()
          store &var_0 <- n0
          jmp node_2

      #node_2:
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

    }
  |}]


let%expect_test "call_with_args" =
  test "./terminators/call/call_with_args.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define call_with_args::call_with_args(x: int, y: int) : int {
      local var_0: int, var_3: int, var_4: int
      #node_0:
          n0:int = load &x
          store &var_3 <- n0
          n1:int = load &y
          store &var_4 <- n1
          n2:int = load &var_3
          n3:int = load &var_4
          n4 = add(n2, n3)
          store &var_0 <- n4
          jmp node_1
        
      #node_1:
          n5:void = load &var_0
          ret n5

    }

    define call_with_args::add(a: int, b: int) : int {
      local var_0: int, var_3: int, var_4: int, var_5: int
      #node_0:
          n0:int = load &a
          store &var_3 <- n0:int
          n1:int = load &b
          store &var_4 <- n1:int
          n2:int = load &var_3
          n3:int = load &var_4
          store &var_5 <- __sil_plusa_int(n2, n3)
          n4:int = load &var_5
          store &var_0 <- n4
          n5:int = load &var_0
          ret n5

    }

    define call_with_args::main() : void {
      local var_0: void, x: int, y: int, var_3: int, var_4: int, var_5: int
      #node_0:
          store &x <- 0:int
          store &y <- 1:int
          n0:int = load &x
          store &var_4 <- n0
          n1:int = load &y
          store &var_5 <- n1
          n2:int = load &var_4
          n3:int = load &var_5
          n4 = call_with_args(n2, n3)
          store &var_3 <- n4
          jmp node_1
      
      #node_1:
          store &var_0 <- null:void
          n5:void = load &var_0
          ret n5

    }

  |}]


(* Tests for drop - TODO: Enable drop tests when the string and box are supported *)

(*
  let%expect_test "drop_return_early" =
  test "./terminators/drop/drop_return_early.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define drop_return_early(flag: bool) : int {
        local var_0: int, var_2: *String, var_3: *int, var_4: int, var_5: int
        #node_0:
            store &var_2 <- "A":*String
            store &var_3 <- 1:int
            jmp node_1

        #node_1:
            n0:int = load &flag
            if n0 then jmp node_2 else jmp node_4

        #node_2:
            n1:int = load &var_3
            store &var_4 <- n1
            n2:int = load &var_4
            n3:int = __sil_free(n2)
            jmp node_3

        #node_3:
            n4:*String = load &var_2
            store &var_5 <- n4
            n5:int = load &var_5
            n6:int = __sil_free(n5)
            jmp node_6

        #node_4:
            n7:*String = load &var_2
            store &var_4 <- n7
            n8:int = load &var_4
            n9:int = __sil_free(n8)
            n10:int = load &var_3
            store &var_5 <- n10
            n11:int = load &var_5
            n12:int = __sil_free(n11)
            jmp node_5

        #node_5:
            ret 0

        #node_6:
            ret 1
            
    }

    define drop_return_early::main() : void {
        local var_0: void, var_1: int, var_2: int
        #node_0:
            store &var_1 <- 0:int
            n0:bool = load &var_1
            store &var_2 <- n0
            n1:int = drop_return_early(n0)
            store &var_0 <- null:void
            n2:void = load &var_0
            ret n2

    }

  |}]


  let%expect_test "drop_implicit" =
    test "./terminators/drop/drop_implicit.ullbc" ;
    [%expect
      {|
      .source_language = "Rust"

      define drop_implicit::drop_implicit() : int {
        local var_0: int, s:*String, b:*int
        #node_0:
            store &s <- "hello":*String
            n0:*String = load &s
            store b <- 42:int
            n1:int = load b
            ret n1
      }

      define drop_implicit::main() : void {
        local var_0: void, var_1: int
        #node_0:
            n0:int = drop_implicit()
            store &var_1 <- n0:int
            store &var_0 <- null:void
            n1:void = load &var_0
            ret n1
      }
    |}]
*)

(* Tests for goto *)
let%expect_test "basic_loop" =
  test "./terminators/goto/basic_loop.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define basic_loop::goto_loop(n: int) : int {
      local var_0: int, var_2: void, var_3: int, var_4: int, var_5: int
      #node_0:
          jmp node_1

      #node_1:
          n0:int = load &n
          store &var_3 <- __sil_plusa_int(n0, 1):int
          n1:int = load &var_3
          store &n <- n1:int
          n2:int = load &n
          store &var_5 <- n2:int
          n3:int = load &var_5
          store &var_4 <- __sil_gt(n3, 5):int
          n4:int = load &var_4
          if n4 then jmp node_2 else jmp node_3

      #node_2:
          n5:int = load &n
          store &var_0 <- n5:int
          n6:int = load &var_0
          ret n6

      #node_3:
          jmp node_1
        
    }

    define basic_loop::main() : void {
      local var_0: void, x: int, var_2: int, var_3: int
      #node_0:
          store &x <- 0:int
          n0:int = load &x
          store &var_3 <- n0:int
          n1:int = load &var_3
          n2:int = goto_loop(n1)
          store &var_2 <- n2:int
          jmp node_1

      #node_1:
          store &var_0 <- null:void
          n3:void = load &var_0
          ret n3

    }

  |}]


let%expect_test "loop_with_continue" =
  test "./terminators/goto/loop_with_continue.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define loop_with_continue::goto_with_continue(x: int, y: int) : int {
      local var_0: int, var_3: void, var_4: void, var_5: int, var_6: int, var_7: int, var_8: int
      #node_0:
          jmp node_1

      #node_1:
          n0:int = load &x
          store &var_6 <- n0:int
          n1:int = load &y
          store &var_7 <- n1:int
          n2:int = load &var_6
          n3:int = load &var_7
          store &var_5 <- __sil_lt(n2, n3)
          n4:int = load &var_5
          if n4 then jmp node_2 else jmp node_3

      #node_2:
          n5:int = load &x
          store &var_8 <- __sil_plusa_int(n5, 1)
          n6:int = load &var_8
          store &x <- n6:int
          jmp node_1

      #node_3:
          n7:int = load &x
          store &var_0 <- n7:int
          n8:int = load &var_0
          ret n8
        
    }

    define loop_with_continue::main() : void {
      local var_0: void, x: int, y: int, var_3: int, var_4: int, var_5: int
      #node_0:
          store &x <- 0:int
          store &y <- 1:int
          n0:int = load &x
          store &var_4 <- n0:int
          n1:int = load &y
          store &var_5 <- n1:int
          n2:int = load &var_4
          n3:int = load &var_5
          n4 = goto_with_continue(n2, n3)
          store &var_3 <- n4:int
          jmp node_1

      #node_1:
          store &var_0 <- null:void
          n5:void = load &var_0
          ret n5

    }
  |}]


(* Tests for switch_int *)
let%expect_test "int_comparison" =
  test "./terminators/switch_int/int_comparison.ullbc" ;
  [%expect
    {|
  .source_language = "Rust"

  define int_comparison::compare(x: int, y: int) : int {
    local var_0: int, var_3: int, var_4: int, var_5: int
    #node_0:
        n0:int = load &x
        store &var_4 <- n0:int
        n1:int = load &y
        store &var_5 <- n1:int
        n2:int = load &var_4
        n3:int = load &var_5
        store &var_3 <- __sil_gt(n2, n3)
        n4:int = load &var_3
        if n4 then jmp node_1 else jmp node_2

    #node_1:
        n5:int = load &x
        store &var_0 <- n5:int
        jmp node_3

    #node_2:
        n6:int = load &y
        store &var_0 <- n6:int
        jmp node_3

    #node_3:
        n7:int = load &var_0
        ret n7

  }

  define int_comparison::main() : void {
    local var_0: void, x: int, y: int, var_3: int, var_4: int, var_5: int
    #node_0:
        store &x <- 0:int
        store &y <- 1:int
        n0:int = load &x
        store &var_4 <- n0:int
        n1:int = load &y
        store &var_5 <- n1:int
        n2:int = load &var_4
        n3:int = load &var_5
        n4 = compare(n2, n3)
        store &var_3 <- n4:int
        jmp node_1

    #node_1:
        store &var_0 <- null:void
        n5:void = load &var_0
        ret n5

  }
  
  |}]


let%expect_test "nested" =
  test "./terminators/switch_int/nested.ullbc" ;
  [%expect
    {|
  .source_language = "Rust"

  define nested::swi_nested(a: int, b: int) : int {
    local var_0: int, var_3: int, var_4: int, var_5: int, var_6: int
    #node_0:
        n0:int = load &a
        store &var_4 <- n0:int
        n1:int = load &var_4
        store &var_3 <- __sil_ge(n1, 0)
        n2:int = load &var_3
        if n2 then jmp node_1 else jmp node_2

    #node_1:
        n3:int = load &b
        store &var_6 <- n3:int
        n4:int = load &var_6
        store &var_5 <- __sil_eq(n4, 0)
        n5:int = load &var_5
        if n5 then jmp node_3 else jmp node_4

    #node_2:
        store &var_0 <- 3:int
        jmp node_5

    #node_3:
        store &var_0 <- 1:int
        jmp node_6

    #node_4:
        store &var_0 <- 2:int
        jmp node_6

    #node_5:
        n6:int = load &var_0
        ret n6

    #node_6:
        jmp node_5

  }

  define nested::main() : void {
    local var_0: void, x: int, y: int, var_3: int, var_4: int, var_5: int
    #node_0:
        store &x <- 0:int
        store &y <- 1:int
        n0:int = load &x
        store &var_4 <- n0:int
        n1:int = load &y
        store &var_5 <- n1:int
        n2:int = load &var_4
        n3:int = load &var_5
        n4 = swi_nested(n2, n3)
        store &var_3 <- n4:int
        jmp node_1

    #node_1:
        store &var_0 <- null:void
        n5:void = load &var_0
        ret n5

  }

  |}]
