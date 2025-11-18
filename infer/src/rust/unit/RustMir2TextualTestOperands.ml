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
  let source = {|
fn main() {
    3.14;
}
    |} in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, var_1: float
      #node_0:
          store &var_1 <- 3.14:float
          store &var_0 <- null:void
          store &var_0 <- null:void
          n0:void = load &var_0
          ret n0

    }

    |}]


let%expect_test "literal_int" =
  let source = {|
fn main() {
    42;
}
    |} in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, var_1: int
      #node_0:
          store &var_1 <- 42:int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n0:void = load &var_0
          ret n0
          
    }

    |}]

let%expect_test "literal_unit" =
  let source = {|
fn main() {
    ();
}
    |} in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, var_1: void
      #node_0:
          store &var_0 <- null:void
          store &var_0 <- null:void
          n0:void = load &var_0
          ret n0
        
    }

    |}]


(* Tests for copy *)
let%expect_test "basic_copy" =
  let source = {|
fn main() {
    let x = 42;
    let y = x;
}
    |} in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, x_1: int, y_2: int
      #node_0:
          store &x_1 <- 42:int
          n0:int = load &x_1
          store &y_2 <- n0:int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1
        
    }

    |}]

let%expect_test "nested_copy" =
  let source = {|
fn main() {
    let x = 100;
    {
        let y = x;
    }
}
    |} in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, x_1: int, y_2: int
      #node_0:
          store &x_1 <- 100:int
          n0:int = load &x_1    
          store &y_2 <- n0:int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n1:void = load &var_0
          ret n1

    }

    |}]