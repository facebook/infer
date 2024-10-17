(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

let%expect_test _ =
  let source =
    {|
x = 42
print(x)
builtin_print = print
print = 0
builtin_print(print, None, "hello world", True, False)
|}
  in
  PyIR.test source ;
  F.printf "Running interpreter:@\n" ;
  PyIR.test ~run:PyIRExec.run source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          TOPLEVEL[x] <- 42
          n0 <- TOPLEVEL[print]
          n1 <- TOPLEVEL[x]
          n2 <- $Call(n0, n1, None)
          n3 <- TOPLEVEL[print]
          TOPLEVEL[builtin_print] <- n3
          TOPLEVEL[print] <- 0
          n4 <- TOPLEVEL[builtin_print]
          n5 <- TOPLEVEL[print]
          n6 <- $Call(n4, n5, None, "hello world", true, false, None)
          return None



    Running interpreter:
    42
    0 None hello world True False |}]
