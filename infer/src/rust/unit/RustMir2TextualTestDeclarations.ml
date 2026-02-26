(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open RustMir2TextualTest

(* An example test *)
let%expect_test "global places" =
  let source =
    {|
const X : i32 = 5;
const Y : i32 = succ(X);


const fn succ(i : i32) -> i32 {
    i + 1
}

#[allow(unused)]
fn main() {
    let eleven = X + Y;
}
|}
  in
  test source ;
  [%expect
    {|
    .source_language = "Rust"

    define dummy::succ(i_1: int) : int {
      local var_0: int, var_2: int, var_3: int
      #node_0:
          n0:int = load &i_1
          store &var_2 <- n0:int
          n1:int = load &var_2
          store &var_3 <- __sil_plusa_int(n1, 1):int
          n2:int = load &var_3
          store &var_0 <- n2:int
          n3:int = load &var_0
          ret n3

    }

    define dummy::main() : void {
      local var_0: void, eleven_1: int, var_2: int
      #node_0:
          n0:int = load &GLOBAL@dummy::X
          n1:int = load &GLOBAL@dummy::Y
          store &var_2 <- __sil_plusa_int(n0, n1):int
          n2:int = load &var_2
          store &eleven_1 <- n2:int
          store &var_0 <- null:void
          store &var_0 <- null:void
          n3:void = load &var_0
          ret n3

    }

    define dummy::X() : int {
      local var_0: int
      #node_0:
          store &var_0 <- 5:int
          n0:int = load &var_0
          ret n0

    }

    define dummy::Y() : int {
      local var_0: int
      #node_0:
          n0:int = load &GLOBAL@dummy::X
          n1 = dummy::succ(n0)
          store &var_0 <- n1:int
          jmp node_2

      #node_2:
          n2:int = load &var_0
          ret n2

    }

    global GLOBAL@dummy::X: int = [Some dummy::X()]

    global GLOBAL@dummy::Y: int = [Some dummy::Y()]
    |}]
