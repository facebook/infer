(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open IStd
open RustMir2TextualTest

let%expect_test "after_lifetime" =
  test "./general/after_lifetime.ullbc" ;
  [%expect
    {|
    .source_language = "Rust"

    define after_lifetime::main() : void {
      local var_0: void, ptr: *int, var_2: void, x: int, var_4: *int, var_5: *int, _z: int
      #node_0:
          store &x <- 50:int
          store &var_5 <- &x:*int
          store &var_4 <- var_5:*int
          n0:*int = load &var_4
          store &ptr <- n0:*int
          n2:*int = load &ptr
          n1:int = load n2
          store &_z <- n1:int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n3:void = load &var_0
          ret n3

    }

    |}]
