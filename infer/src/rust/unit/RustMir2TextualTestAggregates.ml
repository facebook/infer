(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open RustMir2TextualTest

let%expect_test "basic_tuple" =
  let source =
    {|
fn main() {
    let mut x = (1, 2);
    let one = x.0;
    let two = x.1;
    x.0 = two;
    x.1 = one;
}
    |}
  in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, x_1: __infer_rust_tuple_class<int,int>, one_2: int, two_3: int, var_4: int, var_5: int
      #node_0:
          store &x_1.__infer_rust_tuple_class<int,int>.0 <- 1:int
          store &x_1.__infer_rust_tuple_class<int,int>.1 <- 2:int
          n0:int = load &x_1.__infer_rust_tuple_class<int,int>.0
          store &one_2 <- n0:int
          n1:int = load &x_1.__infer_rust_tuple_class<int,int>.1
          store &two_3 <- n1:int
          n2:int = load &two_3
          store &var_4 <- n2:int
          n3:int = load &var_4
          store &x_1.__infer_rust_tuple_class<int,int>.0 <- n3:int
          n4:int = load &one_2
          store &var_5 <- n4:int
          n5:int = load &var_5
          store &x_1.__infer_rust_tuple_class<int,int>.1 <- n5:int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n6:void = load &var_0
          ret n6

    }

    type __infer_rust_tuple_class<int,int> = {0: int; 1: int}
    |}]


let%expect_test "basic_struct" =
  let source =
    {|
fn main() {
    struct Point {
        x: i32,
        y: i32,
    }
    
    let mut p = Point { x: 1, y: 2 };
    let one = p.x;
    let two = p.y;
    p.x = two;
    p.y = one;
}
|}
  in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    type dummy::main::Point = {x: int; y: int}

    define dummy::main() : void {
      local var_0: void, p_1: dummy::main::Point, one_2: int, two_3: int, var_4: int, var_5: int
      #node_0:
          store &p_1.dummy::main::Point.x <- 1:int
          store &p_1.dummy::main::Point.y <- 2:int
          n0:int = load &p_1.dummy::main::Point.x
          store &one_2 <- n0:int
          n1:int = load &p_1.dummy::main::Point.y
          store &two_3 <- n1:int
          n2:int = load &two_3
          store &var_4 <- n2:int
          n3:int = load &var_4
          store &p_1.dummy::main::Point.x <- n3:int
          n4:int = load &one_2
          store &var_5 <- n4:int
          n5:int = load &var_5
          store &p_1.dummy::main::Point.y <- n5:int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n6:void = load &var_0
          ret n6

    }
    |}]


let%expect_test "basic_array" =
  let source =
    {|
fn main() {
    let mut x = [4, 5, 6];
    let four = x[0];
    let five = x[1];
    x[0] = five;
    x[1] = four;
}
|}
  in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, x_1: int[], four_2: int, var_3: int, five_4: int, var_5: int, var_6: int, var_7: int, var_8: int, var_9: int
      #node_0:
          store &x_1[0] <- 4:int
          store &x_1[1] <- 5:int
          store &x_1[2] <- 6:int
          store &var_3 <- 0:int
          n0:int = load &var_3
          n1:int = load &x_1[n0]
          store &four_2 <- n1:int
          store &var_5 <- 1:int
          n2:int = load &var_5
          n3:int = load &x_1[n2]
          store &five_4 <- n3:int
          n4:int = load &five_4
          store &var_6 <- n4:int
          store &var_7 <- 0:int
          n5:int = load &var_7
          n6:int = load &var_6
          store &x_1[n5] <- n6:int
          n7:int = load &four_2
          store &var_8 <- n7:int
          store &var_9 <- 1:int
          n8:int = load &var_9
          n9:int = load &var_8
          store &x_1[n8] <- n9:int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n10:void = load &var_0
          ret n10

    }
    |}]


let%expect_test "distance" =
  let source =
    {|
fn main() {
    let p = Point { x: 1, y: 2 };
    let _ = distance(p);
}

fn distance(p: Point) -> i32 {
    p.x * p.x + p.y * p.y
}

struct Point {
    x: i32,
    y: i32,
}

|}
  in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    type dummy::Point = {x: int; y: int}

    define dummy::main() : void {
      local var_0: void, p_1: dummy::Point, var_2: int, var_3: dummy::Point
      #node_0:
          store &p_1.dummy::Point.x <- 1:int
          store &p_1.dummy::Point.y <- 2:int
          n0:dummy::Point = load &p_1
          store &var_3 <- n0:dummy::Point
          n1:dummy::Point = load &var_3
          n2 = dummy::distance(n1)
          store &var_2 <- n2:int
          jmp node_1

      #node_1:
          store &var_0 <- null:void
          store &var_0 <- null:void
          n3:void = load &var_0
          ret n3

    }

    define dummy::distance(p_1: dummy::Point) : int {
      local var_0: int, var_2: int, var_3: int, var_4: int, var_5: int, var_6: int, var_7: int, var_8: int, var_9: int, var_10: int
      #node_0:
          n0:int = load &p_1.dummy::Point.x
          store &var_3 <- n0:int
          n1:int = load &p_1.dummy::Point.x
          store &var_4 <- n1:int
          n2:int = load &var_3
          n3:int = load &var_4
          store &var_5 <- __sil_mult_int(n2, n3):int
          n4:int = load &var_5
          store &var_2 <- n4:int
          n5:int = load &p_1.dummy::Point.y
          store &var_7 <- n5:int
          n6:int = load &p_1.dummy::Point.y
          store &var_8 <- n6:int
          n7:int = load &var_7
          n8:int = load &var_8
          store &var_9 <- __sil_mult_int(n7, n8):int
          n9:int = load &var_9
          store &var_6 <- n9:int
          n10:int = load &var_2
          n11:int = load &var_6
          store &var_10 <- __sil_plusa_int(n10, n11):int
          n12:int = load &var_10
          store &var_0 <- n12:int
          n13:int = load &var_0
          ret n13

    }
|}]


let%expect_test "empty_struct" =
  let source =
    {|
struct Adder;

impl Adder {
    fn add(&self, a : i32, b : i32) -> i32 {
        a + b
    }
}

fn main() {
    let adder = Adder; // no fields
    let three = adder.add(1,2);
}

|}
  in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    type dummy::Adder = {}

    define dummy::main() : void {
      local var_0: void, adder_1: dummy::Adder, three_2: int, var_3: *dummy::Adder
      #node_0:
          store &adder_1 <- null:void
          store &var_3 <- &adder_1:*dummy::Adder
          n0:*dummy::Adder = load &var_3
          n1 = dummy::{dummy::Adder}::add(n0, 1, 2)
          store &three_2 <- n1:int
          jmp node_1

      #node_1:
          store &var_0 <- null:void
          store &var_0 <- null:void
          n2:void = load &var_0
          ret n2

    }

    define dummy::{dummy::Adder}::add(self_1: *dummy::Adder, a_2: int, b_3: int) : int {
      local var_0: int, var_4: int, var_5: int, var_6: int
      #node_0:
          n0:int = load &a_2
          store &var_4 <- n0:int
          n1:int = load &b_3
          store &var_5 <- n1:int
          n2:int = load &var_4
          n3:int = load &var_5
          store &var_6 <- __sil_plusa_int(n2, n3):int
          n4:int = load &var_6
          store &var_0 <- n4:int
          n5:int = load &var_0
          ret n5

    }
    |}]


let%expect_test "nested_type" =
  let source =
    {|
fn main() {
    let x = Foo { array: [1, 2], tuple: (3, 4) };
}

struct Foo {
    array: [i32; 2],
    tuple: (i32, i32),
}
|}
  in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    type dummy::Foo = {array: int[]; tuple: __infer_rust_tuple_class<int,int>}

    define dummy::main() : void {
      local var_0: void, x_1: dummy::Foo, var_2: int[], var_3: __infer_rust_tuple_class<int,int>
      #node_0:
          store &var_2[0] <- 1:int
          store &var_2[1] <- 2:int
          store &var_3.__infer_rust_tuple_class<int,int>.0 <- 3:int
          store &var_3.__infer_rust_tuple_class<int,int>.1 <- 4:int
          n0:int[] = load &var_2
          store &x_1.dummy::Foo.array <- n0:int[]
          n1:__infer_rust_tuple_class<int,int> = load &var_3
          store &x_1.dummy::Foo.tuple <- n1:__infer_rust_tuple_class<int,int>
          store &var_0 <- null:void
          store &var_0 <- null:void
          n2:void = load &var_0
          ret n2

    }

    type __infer_rust_tuple_class<int,int> = {0: int; 1: int}
    |}]


let%expect_test "multiple_tuples" =
  let source =
    {|
fn main() {
    let x = (1, 2);
    let y = (2, 3);
    let z = ([3],[4]);
    let aa = ((1,2,3),(4,5,6));
}
    |}
  in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, x_1: __infer_rust_tuple_class<int,int>, y_2: __infer_rust_tuple_class<int,int>, z_3: __infer_rust_tuple_class<int[],int[]>, var_4: int[], var_5: int[], aa_6: __infer_rust_tuple_class<__infer_rust_tuple_class<int,int,int>,__infer_rust_tuple_class<int,int,int>>, var_7: __infer_rust_tuple_class<int,int,int>, var_8: __infer_rust_tuple_class<int,int,int>
      #node_0:
          store &x_1.__infer_rust_tuple_class<int,int>.0 <- 1:int
          store &x_1.__infer_rust_tuple_class<int,int>.1 <- 2:int
          store &y_2.__infer_rust_tuple_class<int,int>.0 <- 2:int
          store &y_2.__infer_rust_tuple_class<int,int>.1 <- 3:int
          store &var_4[0] <- 3:int
          store &var_5[0] <- 4:int
          n0:int[] = load &var_4
          store &z_3.__infer_rust_tuple_class<int[],int[]>.0 <- n0:int[]
          n1:int[] = load &var_5
          store &z_3.__infer_rust_tuple_class<int[],int[]>.1 <- n1:int[]
          store &var_7.__infer_rust_tuple_class<int,int,int>.0 <- 1:int
          store &var_7.__infer_rust_tuple_class<int,int,int>.1 <- 2:int
          store &var_7.__infer_rust_tuple_class<int,int,int>.2 <- 3:int
          store &var_8.__infer_rust_tuple_class<int,int,int>.0 <- 4:int
          store &var_8.__infer_rust_tuple_class<int,int,int>.1 <- 5:int
          store &var_8.__infer_rust_tuple_class<int,int,int>.2 <- 6:int
          n2:__infer_rust_tuple_class<int,int,int> = load &var_7
          store &aa_6.__infer_rust_tuple_class<__infer_rust_tuple_class<int,int,int>,__infer_rust_tuple_class<int,int,int>>.0 <- n2:__infer_rust_tuple_class<int,int,int>
          n3:__infer_rust_tuple_class<int,int,int> = load &var_8
          store &aa_6.__infer_rust_tuple_class<__infer_rust_tuple_class<int,int,int>,__infer_rust_tuple_class<int,int,int>>.1 <- n3:__infer_rust_tuple_class<int,int,int>
          store &var_0 <- null:void
          store &var_0 <- null:void
          n4:void = load &var_0
          ret n4

    }

    type __infer_rust_tuple_class<__infer_rust_tuple_class<int,int,int>,__infer_rust_tuple_class<int,int,int>> = {
    0: __infer_rust_tuple_class<int,int,int>; 1: __infer_rust_tuple_class<int,int,int>}

    type __infer_rust_tuple_class<int,int> = {0: int; 1: int}

    type __infer_rust_tuple_class<int,int,int> = {0: int; 1: int; 2: int}

    type __infer_rust_tuple_class<int[],int[]> = {0: int[]; 1: int[]}
    |}]
