(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open RustMir2TextualTest

(* Tests for dereferencing *)
let%expect_test "basic_deref" =
  let source = {|
fn main() {
    let x = 42;
    let ref_x = &x;
    let y = *ref_x; 
}

    |} in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, x_1: int, ref_x_2: *int, y_3: int
      #node_0:
          store &x_1 <- 42:int
          store &ref_x_2 <- &x_1:*int
          n0:*int = load &ref_x_2
          n1:int = load n0
          store &y_3 <- n1:int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n2:void = load &var_0
          ret n2

    }
    |}]


let%expect_test "deref_multiple" =
  let source =
    {|
fn main() {
    let x = 42;
    let ref_1 = &x;           
    let ref_2 = &ref_1;       
    let ref_3 = &ref_2;       
    let y = ***ref_3;
}
    |}
  in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, x_1: int, ref_1_2: *int, ref_2_3: **int, ref_3_4: ***int, y_5: int
      #node_0:
          store &x_1 <- 42:int
          store &ref_1_2 <- &x_1:*int
          store &ref_2_3 <- &ref_1_2:**int
          store &ref_3_4 <- &ref_2_3:***int
          n0:***int = load &ref_3_4
          n1:**int = load n0
          n2:*int = load n1
          n3:int = load n2
          store &y_5 <- n3:int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n4:void = load &var_0
          ret n4

    }
    |}]


(* Tests for mutable raw pointers *)
let%expect_test "basic_mut_raw_ptr" =
  let source = {|
fn main() {
    let mut y = 10;
    let x = &mut y as *mut i32;
}
    |} in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, y_1: int, x_2: *int, var_3: *int
      #node_0:
          store &y_1 <- 10:int
          store &var_3 <- &y_1:*int
          n0:*int = load &var_3
          store &x_2 <- n0:*int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

    }
    
  |}]


(* Tests for mutable references *)
let%expect_test "basic_mut_ref" =
  let source = {|
fn main() {
    let mut y = 10;
    let x = &mut y;
}
    |} in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, y_1: int, x_2: *int
      #node_0:
          store &y_1 <- 10:int
          store &x_2 <- &y_1:*int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n0:void = load &var_0
          ret n0

    }

  |}]


(* Tests for raw pointers  *)
let%expect_test "basic_raw_ptr" =
  let source = {|
fn main() {
    let y = 10;
    let x = &y as *const i32;
}
    |} in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, y_1: int, x_2: *int, var_3: *int
      #node_0:
          store &y_1 <- 10:int
          store &var_3 <- &y_1:*int
          n0:*int = load &var_3
          store &x_2 <- n0:*int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

    }

  |}]


(* Tests for references *)
let%expect_test "basic_ref" =
  let source = {|
fn main() {
    let y = 10;
    let x = &y;
}
    |} in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, y_1: int, x_2: *int
      #node_0:
          store &y_1 <- 10:int
          store &x_2 <- &y_1:*int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n0:void = load &var_0
          ret n0

    }

  |}]


(* Tests for dereference after reference *)
let%expect_test "basic_ref_deref" =
  let source = {|
fn main() {
    let x = 42;
    
    let y = &*&*&x; 
}
    |} in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, x_1: int, y_2: *int, var_3: *int, var_4: *int
      #node_0:
          store &x_1 <- 42:int
          store &var_4 <- &x_1:*int
          n0:*int = load &var_4
          store &var_3 <- n0:*int
          n1:*int = load &var_3
          store &y_2 <- n1:*int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n2:void = load &var_0
          ret n2

    }
    |}]


let%expect_test "basic_deref_ref" =
  let source = {|
fn main() {
    let x = 42;
    let ptr = &x;
    let y = &*ptr; 
}
    |} in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, x_1: int, ptr_2: *int, y_3: *int
      #node_0:
          store &x_1 <- 42:int
          store &ptr_2 <- &x_1:*int
          n0:*int = load &ptr_2
          store &y_3 <- n0:*int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

    }
    |}]


let%expect_test "mutate_through_reference" =
  let source =
    {|
  fn main() {
      let mut x = 10;
      let ptr = &mut x;
      *ptr = 20;
  }
    |}
  in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, x_1: int, ptr_2: *int
      #node_0:
          store &x_1 <- 10:int
          store &ptr_2 <- &x_1:*int
          n0:*int = load &ptr_2
          store n0 <- 20:int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

    }
    |}]
