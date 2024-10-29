(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

(* Tests with exception handling *)

let%expect_test _ =
  let source = {|
try:
      print("TRY BLOCK")
finally:
      print("FINALLY BLOCK")
      |} in
  PyIR.test source ;
  [%expect {|
    IR error: Unsupported opcode: RERAISE |}]


let%expect_test _ =
  let source =
    {|
try:
      print("TRY BLOCK")
finally:
      if foo:
          print("X")
      else:
          print("Y")
      print("FINALLY BLOCK")
print("END")
          |}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Unsupported opcode: RERAISE |}]


let%expect_test _ =
  let source =
    {|
try:
  print("TRY BLOCK")
except:
  print("EXCEPT BLOCK")
print("END")
          |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- TOPLEVEL[print]
          n1 <- $Call(n0, "TRY BLOCK", None)
          jmp b2

        b2:
          n2 <- TOPLEVEL[print]
          n3 <- $Call(n2, "END", None)
          return None |}]


let%expect_test _ =
  let source =
    {|
import os


try:
    page_size = os.sysconf('SC_PAGESIZE')
except (ValueError, AttributeError):
    try:
        page_size = 0
    except (ValueError, AttributeError):
        page_size = 4096
                 |}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Unsupported opcode: JUMP_IF_NOT_EXC_MATCH |}]


let%expect_test _ =
  let source =
    {|
import foo

def f(x):
    for i in x:
        e = foo.Foo()
        try:
            print("yolo")
        finally:
            e.bar()
        |}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Unsupported opcode: RERAISE |}]


let%expect_test _ =
  let source =
    {|
from foo import ERROR

with open("foo", "r") as fp:
    for line in fp:
        try:
            print("TRY")
        except ERROR:
            print("EXCEPT")
        else:
            print("ELSE")
        |}
  in
  PyIR.test ~debug:true source ;
  [%expect {|
    IR error: Unsupported opcode: JUMP_IF_NOT_EXC_MATCH |}]


let%expect_test _ =
  let source =
    {|
TICKS=0

def subhelper():
    global TICKS
    TICKS += 2
    for i in range(2):
        try:
            print("foo")
        except AttributeError:
            TICKS += 3
        |}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Unsupported opcode: JUMP_IF_NOT_EXC_MATCH |}]


let%expect_test _ =
  let source =
    {|
def foo():
          pass

try:
          foo()
except C as c:
          print(c)
          |}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Unsupported opcode: JUMP_IF_NOT_EXC_MATCH |}]


let%expect_test _ =
  let source =
    {|
async def async_with(filename):
    async with open(filename, 'r') as f:
        await f.read()
|}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Unsupported opcode: GEN_START |}]


let%expect_test _ =
  let source =
    {|
def call_finally():
    try:
        read()
    except Exception as e:
        return
|}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Unsupported opcode: JUMP_IF_NOT_EXC_MATCH |}]


let%expect_test _ =
  let source =
    {|
def call_finally_with_break():
    for i in range(100):
        try:
            read()
        except Exception as e:
            break
|}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Unsupported opcode: JUMP_IF_NOT_EXC_MATCH |}]


let%expect_test _ =
  let source = {|
def raise_from(e):
    raise IndexError from e
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["raise_from", "dummy.raise_from", None, None, None, None]
          TOPLEVEL[raise_from] <- n0
          return None


      function dummy.raise_from(e):
        b0:
          n0 <- GLOBAL[IndexError]
          n1 <- LOCAL[e]
          n0.__cause__ <- n1
          throw n0 |}]


let%expect_test _ =
  let source =
    {|
async def foo():
    async with read1(), read2():
        with read3():
            await action()
|}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Unsupported opcode: GEN_START |}]


let%expect_test _ =
  let source =
    {|
async def foo():
    with read1():
        try:
            with read2():
                res = await get()
            return res
        finally:
            do_finally()
|}
  in
  PyIR.test_cfg_skeleton source ;
  PyIR.test source ;
  [%expect
    {|
    dummy
       2        0 LOAD_CONST                        0 (<code object foo>)
                2 LOAD_CONST                        1 ("foo")
                4 MAKE_FUNCTION                     0
                6 STORE_NAME                        0 (foo)
                8 LOAD_CONST                        2 (None)
               10 RETURN_VALUE                      0
    CFG successors:
       0:
    CFG predecessors:
       0:
    topological order: 0

    IR error: Unsupported opcode: GEN_START
    IR error: Unsupported opcode: GEN_START |}]


let%expect_test _ =
  let source =
    {|
def foo():
    num_attempts = 25
    while num_attempts > 0:
        try:
            should_stop, output = stop_conditionx()
            if should_stop:
                return output
        except Exception:
            if retry_on_failure and num_attempts > 1:
                continue
            else:
                raise
        finally:
            num_attempts = num_attempts - 1
    return
|}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Unsupported opcode: JUMP_IF_NOT_EXC_MATCH |}]


let%expect_test _ =
  let source =
    {|
def foo(test):
    try:
        if test:
            return 0
        return 1
    except Exception:
        return 2
    finally:
        return 3
|}
  in
  PyIR.test ~debug:true source ;
  [%expect
    {|
    Translating dummy...
    Building a new node, starting from offset 0
                  []
       2        0 LOAD_CONST                        0 (<code object foo>)
                  [<foo>]
                2 LOAD_CONST                        1 ("foo")
                  [<foo>; "foo"]
                4 MAKE_FUNCTION                     0
                  [n0]
                6 STORE_NAME                        0 (foo)
                  []
                8 LOAD_CONST                        2 (None)
                  [None]
               10 RETURN_VALUE                      0
                  []
    Successors:

    IR error: Unsupported opcode: JUMP_IF_NOT_EXC_MATCH |}]
