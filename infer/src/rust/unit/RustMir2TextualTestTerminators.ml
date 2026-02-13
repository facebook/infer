(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open RustMir2TextualTest

(* Tests for call *)
let%expect_test "basic_call" =
  let source = {|
fn foo() {
    bar()
}

fn bar() {
    
}

fn main() {
    foo()
}
    |} in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::foo() : void {
      local var_0: void
      #node_0:
          n0 = dummy::bar()
          store &var_0 <- n0:void
          jmp node_2

      #node_2:
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

    }

    define dummy::bar() : void {
      local var_0: void
      #node_0:
          store &var_0 <- null:void
          store &var_0 <- null:void
          n0:void = load &var_0
          ret n0

    }

    define dummy::main() : void {
      local var_0: void
      #node_0:
          n0 = dummy::foo()
          store &var_0 <- n0:void
          jmp node_2

      #node_2:
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

    }
    |}]


let%expect_test "basic_if" =
  let source =
    {|
fn main() {
    let mut b = true;
    if b {
        b = false;
    } else {
        b = true;
    }
}
    |}
  in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, b_1: int, var_2: int
      #node_0:
          store &b_1 <- 1:int
          n0:int = load &b_1
          store &var_2 <- n0:int
          n1:int = load &var_2
          jmp node_1, node_2

      #node_1:
          prune n1
          store &b_1 <- 0:int
          store &var_0 <- null:void
          jmp node_3

      #node_2:
          prune __sil_lnot(n1)
          store &b_1 <- 1:int
          store &var_0 <- null:void
          jmp node_3

      #node_3:
          store &var_0 <- null:void
          n2:void = load &var_0
          ret n2

    }
    |}]


let%expect_test "call_with_arg" =
  let source = {|
fn id(x : i32) -> i32 {
    x
}

fn main() {
    let x = id(10);
}
    |} in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::id(x_1: int) : int {
      local var_0: int
      #node_0:
          n0:int = load &x_1
          store &var_0 <- n0:int
          n1:int = load &var_0
          ret n1

    }

    define dummy::main() : void {
      local var_0: void, x_1: int
      #node_0:
          n0 = dummy::id(10)
          store &x_1 <- n0:int
          jmp node_1

      #node_1:
          store &var_0 <- null:void
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

    }
    |}]


let%expect_test "ifelse_function" =
  let source =
    {|
fn ifelse(b : bool, x : i32, y : i32) -> i32 {
    if b {
        x
    } else {
        y
    }
}

fn main() {
    let x = ifelse(true,10,20);
}
    |}
  in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::ifelse(b_1: int, x_2: int, y_3: int) : int {
      local var_0: int, var_4: int
      #node_0:
          n0:int = load &b_1
          store &var_4 <- n0:int
          n1:int = load &var_4
          jmp node_1, node_2

      #node_1:
          prune n1
          n2:int = load &x_2
          store &var_0 <- n2:int
          jmp node_3

      #node_2:
          prune __sil_lnot(n1)
          n3:int = load &y_3
          store &var_0 <- n3:int
          jmp node_3

      #node_3:
          n4:int = load &var_0
          ret n4

    }

    define dummy::main() : void {
      local var_0: void, x_1: int
      #node_0:
          n0 = dummy::ifelse(1, 10, 20)
          store &x_1 <- n0:int
          jmp node_1

      #node_1:
          store &var_0 <- null:void
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

    }
    |}]


let%expect_test "call_with_args" =
  let source =
    {|
fn call_with_args(x: i32, y: i32) -> i32 {
    add(x, y)
}

fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn main() {
    let x: i32 = 0;
    let y: i32 = 1;
    let _ = call_with_args(x, y);
}
    |}
  in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::call_with_args(x_1: int, y_2: int) : int {
      local var_0: int, var_3: int, var_4: int
      #node_0:
          n0:int = load &x_1
          store &var_3 <- n0:int
          n1:int = load &y_2
          store &var_4 <- n1:int
          n2:int = load &var_3
          n3:int = load &var_4
          n4 = dummy::add(n2, n3)
          store &var_0 <- n4:int
          jmp node_1

      #node_1:
          n5:int = load &var_0
          ret n5

    }

    define dummy::add(a_1: int, b_2: int) : int {
      local var_0: int, var_3: int, var_4: int, var_5: int
      #node_0:
          n0:int = load &a_1
          store &var_3 <- n0:int
          n1:int = load &b_2
          store &var_4 <- n1:int
          n2:int = load &var_3
          n3:int = load &var_4
          store &var_5 <- __sil_plusa_int(n2, n3):int
          n4:int = load &var_5
          store &var_0 <- n4:int
          n5:int = load &var_0
          ret n5

    }

    define dummy::main() : void {
      local var_0: void, x_1: int, y_2: int, var_3: int, var_4: int, var_5: int
      #node_0:
          store &x_1 <- 0:int
          store &y_2 <- 1:int
          n0:int = load &x_1
          store &var_4 <- n0:int
          n1:int = load &y_2
          store &var_5 <- n1:int
          n2:int = load &var_4
          n3:int = load &var_5
          n4 = dummy::call_with_args(n2, n3)
          store &var_3 <- n4:int
          jmp node_1

      #node_1:
          store &var_0 <- null:void
          store &var_0 <- null:void
          n5:void = load &var_0
          ret n5

    }
    |}]


(* Tests for goto *)
let%expect_test "basic_loop" =
  let source =
    {|
pub fn goto_loop(mut n: i32) -> i32 {
    loop {
        n += 1;
        if n > 5 {
            break;
        }
    }
    n
}

fn main() {
    let x: i32 = 0;
    let _ = goto_loop(x);
}
    |}
  in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::goto_loop(n_1: int) : int {
      local var_0: int, var_2: void, var_3: int, var_4: int, var_5: int
      #node_0:
          jmp node_1

      #node_1:
          n0:int = load &n_1
          store &var_3 <- __sil_plusa_int(n0, 1):int
          n1:int = load &var_3
          store &n_1 <- n1:int
          n2:int = load &n_1
          store &var_5 <- n2:int
          n3:int = load &var_5
          store &var_4 <- __sil_gt(n3, 5):int
          n4:int = load &var_4
          jmp node_2, node_3

      #node_2:
          prune n4
          n5:int = load &n_1
          store &var_0 <- n5:int
          n6:int = load &var_0
          ret n6

      #node_3:
          prune __sil_lnot(n4)
          jmp node_1

    }

    define dummy::main() : void {
      local var_0: void, x_1: int, var_2: int, var_3: int
      #node_0:
          store &x_1 <- 0:int
          n0:int = load &x_1
          store &var_3 <- n0:int
          n1:int = load &var_3
          n2 = dummy::goto_loop(n1)
          store &var_2 <- n2:int
          jmp node_1

      #node_1:
          store &var_0 <- null:void
          store &var_0 <- null:void
          n3:void = load &var_0
          ret n3

    }
    |}]


let%expect_test "loop_with_continue" =
  let source =
    {|
pub fn goto_with_continue(mut x: i32, y: i32) -> i32 {
    loop {
        if x < y {
            x += 1;
            continue;
        }
        break;
    }
    x
}

fn main() {
    let x: i32 = 0;
    let y: i32 = 1;
    let _ = goto_with_continue(x, y);
}
    |}
  in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::goto_with_continue(x_1: int, y_2: int) : int {
      local var_0: int, var_3: void, var_4: void, var_5: int, var_6: int, var_7: int, var_8: int
      #node_0:
          jmp node_1

      #node_1:
          n0:int = load &x_1
          store &var_6 <- n0:int
          n1:int = load &y_2
          store &var_7 <- n1:int
          n2:int = load &var_6
          n3:int = load &var_7
          store &var_5 <- __sil_lt(n2, n3):int
          n4:int = load &var_5
          jmp node_2, node_3

      #node_2:
          prune n4
          n5:int = load &x_1
          store &var_8 <- __sil_plusa_int(n5, 1):int
          n6:int = load &var_8
          store &x_1 <- n6:int
          jmp node_1

      #node_3:
          prune __sil_lnot(n4)
          n7:int = load &x_1
          store &var_0 <- n7:int
          n8:int = load &var_0
          ret n8

    }

    define dummy::main() : void {
      local var_0: void, x_1: int, y_2: int, var_3: int, var_4: int, var_5: int
      #node_0:
          store &x_1 <- 0:int
          store &y_2 <- 1:int
          n0:int = load &x_1
          store &var_4 <- n0:int
          n1:int = load &y_2
          store &var_5 <- n1:int
          n2:int = load &var_4
          n3:int = load &var_5
          n4 = dummy::goto_with_continue(n2, n3)
          store &var_3 <- n4:int
          jmp node_1

      #node_1:
          store &var_0 <- null:void
          store &var_0 <- null:void
          n5:void = load &var_0
          ret n5

    }
    |}]


(* Tests for switch_int *)
let%expect_test "int_comparison" =
  let source =
    {|
pub fn compare(x: i32, y: i32) -> i32 {
    if x > y { x } else { y }
}

fn main() {
    let x: i32 = 0;
    let y: i32 = 1;
    let _ = compare(x, y);
}
    |}
  in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::compare(x_1: int, y_2: int) : int {
      local var_0: int, var_3: int, var_4: int, var_5: int
      #node_0:
          n0:int = load &x_1
          store &var_4 <- n0:int
          n1:int = load &y_2
          store &var_5 <- n1:int
          n2:int = load &var_4
          n3:int = load &var_5
          store &var_3 <- __sil_gt(n2, n3):int
          n4:int = load &var_3
          jmp node_1, node_2

      #node_1:
          prune n4
          n5:int = load &x_1
          store &var_0 <- n5:int
          jmp node_3

      #node_2:
          prune __sil_lnot(n4)
          n6:int = load &y_2
          store &var_0 <- n6:int
          jmp node_3

      #node_3:
          n7:int = load &var_0
          ret n7

    }

    define dummy::main() : void {
      local var_0: void, x_1: int, y_2: int, var_3: int, var_4: int, var_5: int
      #node_0:
          store &x_1 <- 0:int
          store &y_2 <- 1:int
          n0:int = load &x_1
          store &var_4 <- n0:int
          n1:int = load &y_2
          store &var_5 <- n1:int
          n2:int = load &var_4
          n3:int = load &var_5
          n4 = dummy::compare(n2, n3)
          store &var_3 <- n4:int
          jmp node_1

      #node_1:
          store &var_0 <- null:void
          store &var_0 <- null:void
          n5:void = load &var_0
          ret n5

    }
    |}]


let%expect_test "nested" =
  let source =
    {|
pub fn swi_nested(a: i32, b: i32) -> i32 {
    if a >= 0 {
        if b == 0 { 1 } else { 2 }
    } else {
        3
    }
}

fn main() {
    let x: i32 = 0;
    let y: i32 = 1;
    let _ = swi_nested(x, y);
}
    |}
  in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::swi_nested(a_1: int, b_2: int) : int {
      local var_0: int, var_3: int, var_4: int, var_5: int, var_6: int
      #node_0:
          n0:int = load &a_1
          store &var_4 <- n0:int
          n1:int = load &var_4
          store &var_3 <- __sil_ge(n1, 0):int
          n2:int = load &var_3
          jmp node_1, node_2

      #node_1:
          prune n2
          n3:int = load &b_2
          store &var_6 <- n3:int
          n4:int = load &var_6
          store &var_5 <- __sil_eq(n4, 0):int
          n5:int = load &var_5
          jmp node_3, node_4

      #node_2:
          prune __sil_lnot(n2)
          store &var_0 <- 3:int
          jmp node_5

      #node_3:
          prune n5
          store &var_0 <- 1:int
          jmp node_6

      #node_4:
          prune __sil_lnot(n5)
          store &var_0 <- 2:int
          jmp node_6

      #node_5:
          n6:int = load &var_0
          ret n6

      #node_6:
          jmp node_5

    }

    define dummy::main() : void {
      local var_0: void, x_1: int, y_2: int, var_3: int, var_4: int, var_5: int
      #node_0:
          store &x_1 <- 0:int
          store &y_2 <- 1:int
          n0:int = load &x_1
          store &var_4 <- n0:int
          n1:int = load &y_2
          store &var_5 <- n1:int
          n2:int = load &var_4
          n3:int = load &var_5
          n4 = dummy::swi_nested(n2, n3)
          store &var_3 <- n4:int
          jmp node_1

      #node_1:
          store &var_0 <- null:void
          store &var_0 <- null:void
          n5:void = load &var_0
          ret n5

    }
    |}]
