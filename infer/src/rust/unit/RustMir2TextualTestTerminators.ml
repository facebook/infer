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

(* TODO: Enable after binops are merged *)
(* let%expect_test "call_with_args" =
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
    
    |}] *)

(* Tests for goto *)
(* TODO: Enable after binops are merged *)
(* let%expect_test "basic_loop" =
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

    |}] *)

(* TODO: Enable after binops are merged *)
(* let%expect_test "loop_with_continue" =
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

    |}] *)

(* Tests for switch_int *)
(* TODO: Enable after binops are merged *)
(* let%expect_test "int_comparison" =
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
    
    |}] *)

(* TODO: Enable after binops are merged *)
(* let%expect_test "nested" =
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

    |}] *)
