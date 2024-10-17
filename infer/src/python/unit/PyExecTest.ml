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


let%expect_test _ =
  let source = {|
def fst(y, x):
    return y

x = 'x'
y = 'y'
print("fst(x, y) =", fst(x, y))
|} in
  PyIR.test source ;
  F.printf "Running interpreter:@\n" ;
  PyIR.test ~run:PyIRExec.run source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["fst", "dummy.fst"](None, None, None, None, None)
          TOPLEVEL[fst] <- n0
          TOPLEVEL[x] <- "x"
          TOPLEVEL[y] <- "y"
          n1 <- TOPLEVEL[print]
          n2 <- TOPLEVEL[fst]
          n3 <- TOPLEVEL[x]
          n4 <- TOPLEVEL[y]
          n5 <- $Call(n2, n3, n4, None)
          n6 <- $Call(n1, "fst(x, y) =", n5, None)
          return None


      function dummy.fst(y, x):
        b0:
          n0 <- LOCAL[y]
          return n0



    Running interpreter:
    fst(x, y) = x |}]
