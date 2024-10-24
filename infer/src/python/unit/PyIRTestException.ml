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
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- TOPLEVEL[print]
          n1 <- $Call(n0, "TRY BLOCK", None)
          jmp b1

        b1:
          n2 <- TOPLEVEL[print]
          n3 <- $Call(n2, "FINALLY BLOCK", None)
          jmp b2

        b2:
          return None |}]


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
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- TOPLEVEL[print]
          n1 <- $Call(n0, "TRY BLOCK", None)
          jmp b1

        b1:
          n2 <- TOPLEVEL[foo]
          if n2 then jmp b2 else jmp b3

        b2:
          n5 <- TOPLEVEL[print]
          n6 <- $Call(n5, "X", None)
          jmp b4

        b3:
          n3 <- TOPLEVEL[print]
          n4 <- $Call(n3, "Y", None)
          jmp b4

        b4:
          n7 <- TOPLEVEL[print]
          n8 <- $Call(n7, "FINALLY BLOCK", None)
          jmp b5

        b5:
          n9 <- TOPLEVEL[print]
          n10 <- $Call(n9, "END", None)
          return None |}]


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
          jmp b3

        b3:
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
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $ImportName(os, None, 0)
          TOPLEVEL[os] <- n0
          n1 <- TOPLEVEL[os]
          n2 <- $CallMethod[sysconf](n1, "SC_PAGESIZE", None)
          TOPLEVEL[page_size] <- n2
          jmp b8

        b8:
          return None |}]


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
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $ImportName(foo, None, 0)
          TOPLEVEL[foo] <- n0
          n1 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n1
          return None


      function dummy.f(x):
        b0:
          n0 <- LOCAL[x]
          n1 <- $GetIter(n0, None)
          jmp b1

        b1:
          n2 <- $NextIter(n1, None)
          n3 <- $HasNextIter(n1, None)
          if n3 then jmp b2 else jmp b5

        b2:
          LOCAL[i] <- n2
          n4 <- GLOBAL[foo]
          n5 <- $CallMethod[Foo](n4, None)
          LOCAL[e] <- n5
          n6 <- GLOBAL[print]
          n7 <- $Call(n6, "yolo", None)
          jmp b3

        b3:
          n8 <- LOCAL[e]
          n9 <- $CallMethod[bar](n8, None)
          jmp b4

        b4:
          jmp b1

        b5:
          return None |}]


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
  [%expect
    {|
    Translating dummy...
    Building a new node, starting from offset 0
                  []
       2        0 LOAD_CONST                        0 (0)
                  [0]
                2 LOAD_CONST                        1 (("ERROR"))
                  [0; $BuildTuple("ERROR")]
                4 IMPORT_NAME                       0 (foo)
                  [n0]
                6 IMPORT_FROM                       1 (ERROR)
                  [n0; n1]
                8 STORE_NAME                        1 (ERROR)
                  [n0]
               10 POP_TOP                           0
                  []
       4       12 LOAD_NAME                         2 (open)
                  [n2]
               14 LOAD_CONST                        2 ("foo")
                  [n2; "foo"]
               16 LOAD_CONST                        3 ("r")
                  [n2; "foo"; "r"]
               18 CALL_FUNCTION                     2
                  [n3]
               20 SETUP_WITH                       66
                  [CM(n3).__exit__; n4]
               22 STORE_NAME                        3 (fp)
                  [CM(n3).__exit__]
       5       24 LOAD_NAME                         3 (fp)
                  [CM(n3).__exit__; n5]
               26 GET_ITER                          0
                  [CM(n3).__exit__; n6]
    Successors: 28

    Building a new node, starting from offset 28
                  [CM(n3).__exit__; n6]
         >>>   28 FOR_ITER                         54 (to +54)
                  [CM(n3).__exit__; n6; n7]
    Successors: 30,84

    Building a new node, starting from offset 84
                  [CM(n3).__exit__]
         >>>   84 POP_BLOCK                         0
                  [CM(n3).__exit__]
               86 BEGIN_FINALLY                     0
                  [CM(n3).__exit__; None]
    Successors: 88

    Building a new node, starting from offset 88
                  [CM(n3).__exit__; None]
         >>>   88 WITH_CLEANUP_START                0
                  [None; None; n9]
               90 WITH_CLEANUP_FINISH               0
                  [None]
               92 END_FINALLY                       0
                  []
    Successors: 94

    Building a new node, starting from offset 94
                  []
               94 LOAD_CONST                        7 (None)
                  [None]
               96 RETURN_VALUE                      0
                  []
    Successors:

    Building a new node, starting from offset 30
                  [CM(n3).__exit__; n6; n7]
               30 STORE_NAME                        4 (line)
                  [CM(n3).__exit__; n6]
       6       32 SETUP_FINALLY                    12
                  [CM(n3).__exit__; n6]
       7       34 LOAD_NAME                         5 (print)
                  [CM(n3).__exit__; n6; n10]
               36 LOAD_CONST                        4 ("TRY")
                  [CM(n3).__exit__; n6; n10; "TRY"]
               38 CALL_FUNCTION                     1
                  [CM(n3).__exit__; n6; n11]
               40 POP_TOP                           0
                  [CM(n3).__exit__; n6]
               42 POP_BLOCK                         0
                  [CM(n3).__exit__; n6]
               44 JUMP_FORWARD                     28 (to +28)
                  [CM(n3).__exit__; n6]
    Successors: 74

    Building a new node, starting from offset 74
                  [CM(n3).__exit__; n6]
      11 >>>   74 LOAD_NAME                         5 (print)
                  [CM(n3).__exit__; n6; n12]
               76 LOAD_CONST                        6 ("ELSE")
                  [CM(n3).__exit__; n6; n12; "ELSE"]
               78 CALL_FUNCTION                     1
                  [CM(n3).__exit__; n6; n13]
               80 POP_TOP                           0
                  [CM(n3).__exit__; n6]
               82 JUMP_ABSOLUTE                    28 (to 28)
                  [CM(n3).__exit__; n6]
    Successors: 28


    module dummy:

      function toplevel():
        b0:
          n0 <- $ImportName(foo, $BuildTuple("ERROR"), 0)
          n1 <- $ImportFrom(ERROR, n0)
          TOPLEVEL[ERROR] <- n1
          n2 <- TOPLEVEL[open]
          n3 <- $Call(n2, "foo", "r", None)
          n4 <- $CallMethod[__enter__](n3, None)
          TOPLEVEL[fp] <- n4
          n5 <- TOPLEVEL[fp]
          n6 <- $GetIter(n5, None)
          jmp b1

        b1:
          n7 <- $NextIter(n6, None)
          n8 <- $HasNextIter(n6, None)
          if n8 then jmp b2 else jmp b7

        b2:
          TOPLEVEL[line] <- n7
          n10 <- TOPLEVEL[print]
          n11 <- $Call(n10, "TRY", None)
          jmp b6

        b6:
          n12 <- TOPLEVEL[print]
          n13 <- $Call(n12, "ELSE", None)
          jmp b1

        b7:
          jmp b8

        b8:
          n9 <- $CallMethod[__enter__](n3, None, None, None, None)
          jmp b9

        b9:
          return None |}]


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
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          GLOBAL[TICKS] <- 0
          n0 <- $MakeFunction["subhelper", "dummy.subhelper", None, None, None, None]
          TOPLEVEL[subhelper] <- n0
          return None


      function dummy.subhelper(i):
        b0:
          n0 <- GLOBAL[TICKS]
          n1 <- $Inplace.Add(n0, 2, None)
          GLOBAL[TICKS] <- n1
          n2 <- GLOBAL[range]
          n3 <- $Call(n2, 2, None)
          n4 <- $GetIter(n3, None)
          jmp b1

        b1:
          n5 <- $NextIter(n4, None)
          n6 <- $HasNextIter(n4, None)
          if n6 then jmp b2 else jmp b7

        b2:
          LOCAL[i] <- n5
          n7 <- GLOBAL[print]
          n8 <- $Call(n7, "foo", None)
          jmp b1

        b7:
          return None |}]


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
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["foo", "dummy.foo", None, None, None, None]
          TOPLEVEL[foo] <- n0
          n1 <- TOPLEVEL[foo]
          n2 <- $Call(n1, None)
          jmp b6

        b6:
          return None


      function dummy.foo():
        b0:
          return None |}]


let%expect_test _ =
  let source =
    {|
async def async_with(filename):
    async with open(filename, 'r') as f:
        await f.read()
|}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["async_with", "dummy.async_with", None, None, None, None]
          TOPLEVEL[async_with] <- n0
          return None


      function dummy.async_with(filename):
        b0:
          n0 <- GLOBAL[open]
          n1 <- LOCAL[filename]
          n2 <- $Call(n0, n1, "r", None)
          n3 <- $CallMethod[__enter__](n2, None)
          n4 <- $GetAwaitable(n3, None)
          n5 <- $YieldFrom(n4, None, None)
          LOCAL[f] <- n4
          n6 <- LOCAL[f]
          n7 <- $CallMethod[read](n6, None)
          n8 <- $GetAwaitable(n7, None)
          n9 <- $YieldFrom(n8, None, None)
          jmp b1

        b1:
          n10 <- $CallMethod[__enter__](n2, None, None, None, None)
          n11 <- $GetAwaitable(n10, None)
          n12 <- $YieldFrom(n11, None, None)
          jmp b2

        b2:
          return None |}]


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
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["call_finally", "dummy.call_finally", None, None, None, None]
          TOPLEVEL[call_finally] <- n0
          return None


      function dummy.call_finally(e):
        b0:
          n0 <- GLOBAL[read]
          n1 <- $Call(n0, None)
          jmp b7

        b7:
          return None |}]


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
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["call_finally_with_break", "dummy.call_finally_with_break", None, None, None, None]
          TOPLEVEL[call_finally_with_break] <- n0
          return None


      function dummy.call_finally_with_break(i, e):
        b0:
          n0 <- GLOBAL[range]
          n1 <- $Call(n0, 100, None)
          n2 <- $GetIter(n1, None)
          jmp b1

        b1:
          n3 <- $NextIter(n2, None)
          n4 <- $HasNextIter(n2, None)
          if n4 then jmp b2 else jmp b11

        b11:
          return None

        b2:
          LOCAL[i] <- n3
          n5 <- GLOBAL[read]
          n6 <- $Call(n5, None)
          jmp b1 |}]


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
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["foo", "dummy.foo", None, None, None, None]
          TOPLEVEL[foo] <- n0
          return None


      function dummy.foo():
        b0:
          n0 <- GLOBAL[read1]
          n1 <- $Call(n0, None)
          n2 <- $CallMethod[__enter__](n1, None)
          n3 <- $GetAwaitable(n2, None)
          n4 <- $YieldFrom(n3, None, None)
          n5 <- GLOBAL[read2]
          n6 <- $Call(n5, None)
          n7 <- $CallMethod[__enter__](n6, None)
          n8 <- $GetAwaitable(n7, None)
          n9 <- $YieldFrom(n8, None, None)
          n10 <- GLOBAL[read3]
          n11 <- $Call(n10, None)
          n12 <- $CallMethod[__enter__](n11, None)
          n13 <- GLOBAL[action]
          n14 <- $Call(n13, None)
          n15 <- $GetAwaitable(n14, None)
          n16 <- $YieldFrom(n15, None, None)
          jmp b1

        b1:
          n17 <- $CallMethod[__enter__](n11, None, None, None, None)
          jmp b2

        b2:
          jmp b3

        b3:
          n18 <- $CallMethod[__enter__](n6, None, None, None, None)
          n19 <- $GetAwaitable(n18, None)
          n20 <- $YieldFrom(n19, None, None)
          jmp b4

        b4:
          jmp b5

        b5:
          n21 <- $CallMethod[__enter__](n1, None, None, None, None)
          n22 <- $GetAwaitable(n21, None)
          n23 <- $YieldFrom(n22, None, None)
          jmp b6

        b6:
          return None |}]


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

    dummy.foo
       3        0 LOAD_GLOBAL                       0 (read1)
                2 CALL_FUNCTION                     0
                4 SETUP_WITH                       66
                6 POP_TOP                           0
       4        8 SETUP_FINALLY                    50
       5       10 LOAD_GLOBAL                       2 (read2)
               12 CALL_FUNCTION                     0
               14 SETUP_WITH                       18
               16 POP_TOP                           0
       6       18 LOAD_GLOBAL                       3 (get)
               20 CALL_FUNCTION                     0
               22 GET_AWAITABLE                     0
               24 LOAD_CONST                        0 (None)
               26 YIELD_FROM                        0
               28 STORE_FAST                        0 (res)
               30 POP_BLOCK                         0
               32 BEGIN_FINALLY                     0
         >>>   34 WITH_CLEANUP_START                0
               36 WITH_CLEANUP_FINISH               0
               38 END_FINALLY                       0
       7       40 LOAD_FAST                         0 (res)
               42 POP_BLOCK                         0
               44 CALL_FINALLY                     14
               46 POP_BLOCK                         0
               48 ROT_TWO                           0
               50 BEGIN_FINALLY                     0
               52 WITH_CLEANUP_START                0
               54 WITH_CLEANUP_FINISH               0
               56 POP_FINALLY                       0
               58 RETURN_VALUE                      0
       9 >>>   60 LOAD_GLOBAL                       1 (do_finally)
               62 CALL_FUNCTION                     0
               64 POP_TOP                           0
               66 END_FINALLY                       0
               68 POP_BLOCK                         0
               70 BEGIN_FINALLY                     0
         >>>   72 WITH_CLEANUP_START                0
               74 WITH_CLEANUP_FINISH               0
               76 END_FINALLY                       0
               78 LOAD_CONST                        0 (None)
               80 RETURN_VALUE                      0
    CFG successors:
       0: 34
      34: 40
      40: 60
      46: 52
      52:
      60: 46
      68: 72
      72: 78
      78:
    CFG predecessors:
       0:
      34: 0
      40: 34
      46: 60
      52: 46
      60: 40
      68:
      72:
      78:
    topological order: 0 34 40 60 46 52

    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["foo", "dummy.foo", None, None, None, None]
          TOPLEVEL[foo] <- n0
          return None


      function dummy.foo(res):
        b0:
          n0 <- GLOBAL[read1]
          n1 <- $Call(n0, None)
          n2 <- $CallMethod[__enter__](n1, None)
          n3 <- GLOBAL[read2]
          n4 <- $Call(n3, None)
          n5 <- $CallMethod[__enter__](n4, None)
          n6 <- GLOBAL[get]
          n7 <- $Call(n6, None)
          n8 <- $GetAwaitable(n7, None)
          n9 <- $YieldFrom(n8, None, None)
          LOCAL[res] <- n8
          jmp b1

        b1:
          n10 <- $CallMethod[__enter__](n4, None, None, None, None)
          jmp b2

        b2:
          n11 <- LOCAL[res]
          jmp b5

        b3:
          jmp b4

        b4:
          n14 <- $CallMethod[__enter__](n1, None, None, None, None)
          return n11

        b5:
          n12 <- GLOBAL[do_finally]
          n13 <- $Call(n12, None)
          jmp b3 |}]


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
  [%expect
    {| IR error: bad operand stack: offset 92 is reachable with two stacks of different sizes |}]


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

    Translating dummy.foo...
    Building a new node, starting from offset 0
                  []
       3        0 LOAD_CONST                        0 (None)
                  [None]
                2 SETUP_FINALLY                    62
                  [None]
                4 SETUP_FINALLY                    28
                  [None]
       4        6 LOAD_FAST                         0 (test)
                  [None; n0]
                8 POP_JUMP_IF_FALSE                22 (to 22)
                  [None]
    Successors: 10,22

    Building a new node, starting from offset 22
                  [None]
       6 >>>   22 POP_BLOCK                         0
                  [None]
               24 POP_BLOCK                         0
                  [None]
               26 CALL_FINALLY                     38
                  [None; CFR(28)]
    Successors: 66

    Building a new node, starting from offset 10
                  [None]
       5       10 POP_BLOCK                         0
                  [None]
               12 POP_BLOCK                         0
                  [None]
               14 CALL_FINALLY                     50
                  [None; CFR(16)]
    Successors: 66

    Building a new node, starting from offset 66 with params (n1)
                  [None; n1]
      10 >>>   66 POP_FINALLY                       0
                  [None]
               68 POP_TOP                           0
                  []
               70 LOAD_CONST                        1 (3)
                  [3]
               72 RETURN_VALUE                      0
                  []
    Successors:

    IR error: UNEXPECTED_EXPRESSION: CFR(16) |}]
