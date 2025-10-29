(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open IStd
open RustMir2TextualTest

(* Tests for storage dead and storage live *)
let%expect_test "shadowing" =
  let source =
    {|
fn main() {
    let x = 1;
    {
        let x = 2;
        let _ = x;
    }
    let _ = x;
}
    |}
  in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::main() : void {
      local var_0: void, x_1: int, var_2: void, x_3: int
      #node_0:
          store &x_1 <- 1:int
          store &x_3 <- 2:int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n0:void = load &var_0
          ret n0

    }

  |}]
