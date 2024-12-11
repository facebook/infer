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
          n0 <- None
          n3 <- TOPLEVEL[print]
          n4 <- $Call(n3, "TRY BLOCK", n0)
          n5 <- TOPLEVEL[print]
          n6 <- $Call(n5, "FINALLY BLOCK", n0)
          return n0 |}]


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
          n0 <- None
          n3 <- TOPLEVEL[print]
          n4 <- $Call(n3, "TRY BLOCK", n0)
          n5 <- TOPLEVEL[foo]
          if n5 then jmp b1 else jmp b2

        b1:
          n8 <- TOPLEVEL[print]
          n9 <- $Call(n8, "X", n0)
          jmp b3

        b2:
          n6 <- TOPLEVEL[print]
          n7 <- $Call(n6, "Y", n0)
          jmp b3

        b3:
          n10 <- TOPLEVEL[print]
          n11 <- $Call(n10, "FINALLY BLOCK", n0)
          jmp b8

        b8:
          n12 <- TOPLEVEL[print]
          n13 <- $Call(n12, "END", n0)
          return n0 |}]


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
          n0 <- None
          n3 <- TOPLEVEL[print]
          n4 <- $Call(n3, "TRY BLOCK", n0)
          jmp b2

        b2:
          n5 <- TOPLEVEL[print]
          n6 <- $Call(n5, "END", n0)
          return n0 |}]


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
          n0 <- None
          n3 <- $ImportName(os, n0, 0)
          TOPLEVEL[os] <- n3
          n4 <- TOPLEVEL[os]
          n5 <- $CallMethod[sysconf](n4, "SC_PAGESIZE", n0)
          TOPLEVEL[page_size] <- n5
          return n0 |}]


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
          n0 <- None
          n3 <- $ImportName(foo, n0, 0)
          TOPLEVEL[foo] <- n3
          n4 <- $MakeFunction["f", "dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n4
          return n0


      function dummy.f(x):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          n4 <- $GetIter(n3, n0)
          jmp b1

        b1:
          n5 <- $NextIter(n4, n0)
          n6 <- $HasNextIter(n4, n0)
          if n6 then jmp b2 else jmp b4

        b2:
          LOCAL[i] <- n5
          n7 <- GLOBAL[foo]
          n8 <- $CallMethod[Foo](n7, n0)
          LOCAL[e] <- n8
          n9 <- GLOBAL[print]
          n10 <- $Call(n9, "yolo", n0)
          n11 <- LOCAL[e]
          n12 <- $CallMethod[bar](n11, n0)
          jmp b1

        b4:
          return n0 |}]


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
                  [n3]
                6 IMPORT_FROM                       1 (ERROR)
                  [n3; n4]
                8 STORE_NAME                        1 (ERROR)
                  [n3]
               10 POP_TOP                           0
                  []
       4       12 LOAD_NAME                         2 (open)
                  [n5]
               14 LOAD_CONST                        2 ("foo")
                  [n5; "foo"]
               16 LOAD_CONST                        3 ("r")
                  [n5; "foo"; "r"]
               18 CALL_FUNCTION                     2
                  [n6]
               20 SETUP_WITH                       38
                  [CM(n6).__exit__; n7]
               22 STORE_NAME                        3 (fp)
                  [CM(n6).__exit__]
       5       24 LOAD_NAME                         3 (fp)
                  [CM(n6).__exit__; n8]
               26 GET_ITER                          0
                  [CM(n6).__exit__; n9]
    Successors: 28

    Building a new node, starting from offset 28
                  [CM(n6).__exit__; n9]
         >>>   28 FOR_ITER                         26 (to +52)
                  [CM(n6).__exit__; n9; n10]
    Successors: 30,82

    Building a new node, starting from offset 82
                  [CM(n6).__exit__]
       5 >>>   82 POP_BLOCK                         0
                  [CM(n6).__exit__]
       4       84 LOAD_CONST                        7 (None)
                  [CM(n6).__exit__; n0]
               86 DUP_TOP                           0
                  [CM(n6).__exit__; n0; n0]
               88 DUP_TOP                           0
                  [CM(n6).__exit__; n0; n0; n0]
               90 CALL_FUNCTION                     3
                  [n12]
               92 POP_TOP                           0
                  []
               94 LOAD_CONST                        7 (None)
                  [n0]
               96 RETURN_VALUE                      0
                  []
    Successors:

    Building a new node, starting from offset 30
                  [CM(n6).__exit__; n9; n10]
               30 STORE_NAME                        4 (line)
                  [CM(n6).__exit__; n9]
       6       32 SETUP_FINALLY                     6
                  [CM(n6).__exit__; n9]
       7       34 LOAD_NAME                         5 (print)
                  [CM(n6).__exit__; n9; n13]
               36 LOAD_CONST                        4 ("TRY")
                  [CM(n6).__exit__; n9; n13; "TRY"]
               38 CALL_FUNCTION                     1
                  [CM(n6).__exit__; n9; n14]
               40 POP_TOP                           0
                  [CM(n6).__exit__; n9]
               42 POP_BLOCK                         0
                  [CM(n6).__exit__; n9]
               44 JUMP_FORWARD                     13 (to +26)
                  [CM(n6).__exit__; n9]
    Successors: 72

    Building a new node, starting from offset 72
                  [CM(n6).__exit__; n9]
      11 >>>   72 LOAD_NAME                         5 (print)
                  [CM(n6).__exit__; n9; n15]
               74 LOAD_CONST                        6 ("ELSE")
                  [CM(n6).__exit__; n9; n15; "ELSE"]
               76 CALL_FUNCTION                     1
                  [CM(n6).__exit__; n9; n16]
               78 POP_TOP                           0
                  [CM(n6).__exit__; n9]
               80 JUMP_ABSOLUTE                    14 (to 28)
                  [CM(n6).__exit__; n9]
    Successors: 28


    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $ImportName(foo, $BuildTuple("ERROR"), 0)
          n4 <- $ImportFrom(ERROR, n3)
          TOPLEVEL[ERROR] <- n4
          n5 <- TOPLEVEL[open]
          n6 <- $Call(n5, "foo", "r", n0)
          n7 <- $CallMethod[__enter__](n6, n0)
          TOPLEVEL[fp] <- n7
          n8 <- TOPLEVEL[fp]
          n9 <- $GetIter(n8, n0)
          jmp b1

        b1:
          n10 <- $NextIter(n9, n0)
          n11 <- $HasNextIter(n9, n0)
          if n11 then jmp b2 else jmp b7

        b2:
          TOPLEVEL[line] <- n10
          n13 <- TOPLEVEL[print]
          n14 <- $Call(n13, "TRY", n0)
          jmp b6

        b6:
          n15 <- TOPLEVEL[print]
          n16 <- $Call(n15, "ELSE", n0)
          jmp b1

        b7:
          n12 <- $CallMethod[__exit__](n6, n0)
          return n0 |}]


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
          n0 <- None
          GLOBAL[TICKS] <- 0
          n3 <- $MakeFunction["subhelper", "dummy.subhelper", n0, n0, n0, n0]
          TOPLEVEL[subhelper] <- n3
          return n0


      function dummy.subhelper(i):
        b0:
          n0 <- None
          n3 <- GLOBAL[TICKS]
          n4 <- $Inplace.Add(n3, 2, n0)
          GLOBAL[TICKS] <- n4
          n5 <- GLOBAL[range]
          n6 <- $Call(n5, 2, n0)
          n7 <- $GetIter(n6, n0)
          jmp b1

        b1:
          n8 <- $NextIter(n7, n0)
          n9 <- $HasNextIter(n7, n0)
          if n9 then jmp b2 else jmp b6

        b2:
          LOCAL[i] <- n8
          n10 <- GLOBAL[print]
          n11 <- $Call(n10, "foo", n0)
          jmp b1

        b6:
          return n0 |}]


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
          n0 <- None
          n3 <- $MakeFunction["foo", "dummy.foo", n0, n0, n0, n0]
          TOPLEVEL[foo] <- n3
          n4 <- TOPLEVEL[foo]
          n5 <- $Call(n4, n0)
          return n0


      function dummy.foo():
        b0:
          n0 <- None
          return n0 |}]


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
          n0 <- None
          n3 <- $MakeFunction["async_with", "dummy.async_with", n0, n0, n0, n0]
          TOPLEVEL[async_with] <- n3
          return n0


      async function dummy.async_with(filename):
        b0:
          n0 <- None
          $GenStartCoroutine()
          n3 <- GLOBAL[open]
          n4 <- LOCAL[filename]
          n5 <- $Call(n3, n4, "r", n0)
          n6 <- $CallMethod[__enter__](n5, n0)
          n7 <- $GetAwaitable(n6, n0)
          n8 <- $YieldFrom(n7, n0, n0)
          LOCAL[f] <- n7
          n9 <- LOCAL[f]
          n10 <- $CallMethod[read](n9, n0)
          n11 <- $GetAwaitable(n10, n0)
          n12 <- $YieldFrom(n11, n0, n0)
          n13 <- $CallMethod[__exit__](n5, n0)
          n14 <- $GetAwaitable(n13, n0)
          n15 <- $YieldFrom(n14, n0, n0)
          return n0 |}]


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
          n0 <- None
          n3 <- $MakeFunction["call_finally", "dummy.call_finally", n0, n0, n0, n0]
          TOPLEVEL[call_finally] <- n3
          return n0


      function dummy.call_finally(e):
        b0:
          n0 <- None
          n3 <- GLOBAL[read]
          n4 <- $Call(n3, n0)
          return n0 |}]


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
          n0 <- None
          n3 <- $MakeFunction["call_finally_with_break", "dummy.call_finally_with_break", n0, n0, n0, n0]
          TOPLEVEL[call_finally_with_break] <- n3
          return n0


      function dummy.call_finally_with_break(i, e):
        b0:
          n0 <- None
          n3 <- GLOBAL[range]
          n4 <- $Call(n3, 100, n0)
          n5 <- $GetIter(n4, n0)
          jmp b1

        b1:
          n6 <- $NextIter(n5, n0)
          n7 <- $HasNextIter(n5, n0)
          if n7 then jmp b2 else jmp b7

        b2:
          LOCAL[i] <- n6
          n8 <- GLOBAL[read]
          n9 <- $Call(n8, n0)
          jmp b1

        b7:
          return n0 |}]


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
          n0 <- None
          n3 <- $MakeFunction["raise_from", "dummy.raise_from", n0, n0, n0, n0]
          TOPLEVEL[raise_from] <- n3
          return n0


      function dummy.raise_from(e):
        b0:
          n0 <- None
          n3 <- GLOBAL[IndexError]
          n4 <- LOCAL[e]
          n3.__cause__ <- n4
          throw n3 |}]


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
          n0 <- None
          n3 <- $MakeFunction["foo", "dummy.foo", n0, n0, n0, n0]
          TOPLEVEL[foo] <- n3
          return n0


      async function dummy.foo():
        b0:
          n0 <- None
          $GenStartCoroutine()
          n3 <- GLOBAL[read1]
          n4 <- $Call(n3, n0)
          n5 <- $CallMethod[__enter__](n4, n0)
          n6 <- $GetAwaitable(n5, n0)
          n7 <- $YieldFrom(n6, n0, n0)
          n8 <- GLOBAL[read2]
          n9 <- $Call(n8, n0)
          n10 <- $CallMethod[__enter__](n9, n0)
          n11 <- $GetAwaitable(n10, n0)
          n12 <- $YieldFrom(n11, n0, n0)
          n13 <- GLOBAL[read3]
          n14 <- $Call(n13, n0)
          n15 <- $CallMethod[__enter__](n14, n0)
          n16 <- GLOBAL[action]
          n17 <- $Call(n16, n0)
          n18 <- $GetAwaitable(n17, n0)
          n19 <- $YieldFrom(n18, n0, n0)
          n20 <- $CallMethod[__exit__](n14, n0)
          jmp b4

        b4:
          n21 <- $CallMethod[__exit__](n9, n0)
          n22 <- $GetAwaitable(n21, n0)
          n23 <- $YieldFrom(n22, n0, n0)
          jmp b8

        b8:
          n24 <- $CallMethod[__exit__](n4, n0)
          n25 <- $GetAwaitable(n24, n0)
          n26 <- $YieldFrom(n25, n0, n0)
          return n0 |}]


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
                0 GEN_START                         1
       3        2 LOAD_GLOBAL                       0 (read1)
                4 CALL_FUNCTION                     0
                6 SETUP_WITH                       44
                8 POP_TOP                           0
       4       10 SETUP_FINALLY                    38
       5       12 LOAD_GLOBAL                       1 (read2)
               14 CALL_FUNCTION                     0
               16 SETUP_WITH                       14
               18 POP_TOP                           0
       6       20 LOAD_GLOBAL                       2 (get)
               22 CALL_FUNCTION                     0
               24 GET_AWAITABLE                     0
               26 LOAD_CONST                        0 (None)
               28 YIELD_FROM                        0
               30 STORE_FAST                        0 (res)
               32 POP_BLOCK                         0
       5       34 LOAD_CONST                        0 (None)
               36 DUP_TOP                           0
               38 DUP_TOP                           0
               40 CALL_FUNCTION                     3
               42 POP_TOP                           0
               44 JUMP_FORWARD                      8 (to +16)
         >>>   46 WITH_EXCEPT_START                 0
               48 POP_JUMP_IF_TRUE                 26 (to 52)
               50 RERAISE                           1
         >>>   52 POP_TOP                           0
               54 POP_TOP                           0
               56 POP_TOP                           0
               58 POP_EXCEPT                        0
               60 POP_TOP                           0
       7 >>>   62 LOAD_FAST                         0 (res)
               64 POP_BLOCK                         0
       9       66 LOAD_GLOBAL                       3 (do_finally)
               68 CALL_FUNCTION                     0
               70 POP_TOP                           0
       3       72 POP_BLOCK                         0
               74 ROT_TWO                           0
               76 LOAD_CONST                        0 (None)
               78 DUP_TOP                           0
               80 DUP_TOP                           0
               82 CALL_FUNCTION                     3
               84 POP_TOP                           0
               86 RETURN_VALUE                      0
       9 >>>   88 LOAD_GLOBAL                       3 (do_finally)
               90 CALL_FUNCTION                     0
               92 POP_TOP                           0
               94 RERAISE                           0
       3 >>>   96 WITH_EXCEPT_START                 0
               98 POP_JUMP_IF_TRUE                 51 (to 102)
              100 RERAISE                           1
         >>>  102 POP_TOP                           0
              104 POP_TOP                           0
              106 POP_TOP                           0
              108 POP_EXCEPT                        0
              110 POP_TOP                           0
              112 LOAD_CONST                        0 (None)
              114 RETURN_VALUE                      0
    CFG successors:
       0: 62
      46: 50 52
      50: 52
      52: 62
      62:
      88: 96
      96: 100 102
     100: 102
     102:
    CFG predecessors:
       0:
      46:
      50:
      52:
      62: 0
      88:
      96:
     100:
     102:
    topological order: 0 62

    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["foo", "dummy.foo", n0, n0, n0, n0]
          TOPLEVEL[foo] <- n3
          return n0


      async function dummy.foo(res):
        b0:
          n0 <- None
          $GenStartCoroutine()
          n3 <- GLOBAL[read1]
          n4 <- $Call(n3, n0)
          n5 <- $CallMethod[__enter__](n4, n0)
          n6 <- GLOBAL[read2]
          n7 <- $Call(n6, n0)
          n8 <- $CallMethod[__enter__](n7, n0)
          n9 <- GLOBAL[get]
          n10 <- $Call(n9, n0)
          n11 <- $GetAwaitable(n10, n0)
          n12 <- $YieldFrom(n11, n0, n0)
          LOCAL[res] <- n11
          n13 <- $CallMethod[__exit__](n7, n0)
          jmp b4

        b4:
          n14 <- LOCAL[res]
          n15 <- GLOBAL[do_finally]
          n16 <- $Call(n15, n0)
          n17 <- $CallMethod[__exit__](n4, n0)
          return n14 |}]


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
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["foo", "dummy.foo", n0, n0, n0, n0]
          TOPLEVEL[foo] <- n3
          return n0


      function dummy.foo(num_attempts, should_stop, output):
        b0:
          n0 <- None
          LOCAL[num_attempts] <- 25
          jmp b1

        b1:
          n3 <- LOCAL[num_attempts]
          n4 <- $Compare.gt(n3, 0, n0)
          if n4 then jmp b2 else jmp b14

        b11:
          n8 <- LOCAL[num_attempts]
          n9 <- $Binary.Subtract(n8, 1, n0)
          LOCAL[num_attempts] <- n9
          jmp b13

        b13:
          n10 <- LOCAL[num_attempts]
          n11 <- $Compare.gt(n10, 0, n0)
          if n11 then jmp b2 else jmp b14

        b14:
          return n0

        b2:
          n5 <- GLOBAL[stop_conditionx]
          n6 <- $Call(n5, n0)
          LOCAL[should_stop] <- n6[0]
          LOCAL[output] <- n6[1]
          n7 <- LOCAL[should_stop]
          if n7 then jmp b3 else jmp b4

        b3:
          n12 <- LOCAL[output]
          n13 <- LOCAL[num_attempts]
          n14 <- $Binary.Subtract(n13, 1, n0)
          LOCAL[num_attempts] <- n14
          return n12

        b4:
          jmp b11 |}]


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
                  [n3]
                6 STORE_NAME                        0 (foo)
                  []
                8 LOAD_CONST                        2 (None)
                  [n0]
               10 RETURN_VALUE                      0
                  []
    Successors:

    Translating dummy.foo...
    Building a new node, starting from offset 0
                  []
       3        0 SETUP_FINALLY                    22
                  []
                2 SETUP_FINALLY                    10
                  []
       4        4 LOAD_FAST                         0 (test)
                  [n3]
                6 POP_JUMP_IF_FALSE                 8 (to 16)
                  []
    Successors: 8,16

    Building a new node, starting from offset 16
                  []
       6 >>>   16 POP_BLOCK                         0
                  []
               18 POP_BLOCK                         0
                  []
      10       20 LOAD_CONST                        1 (3)
                  [3]
               22 RETURN_VALUE                      0
                  []
    Successors:

    Building a new node, starting from offset 8
                  []
       5        8 POP_BLOCK                         0
                  []
               10 POP_BLOCK                         0
                  []
      10       12 LOAD_CONST                        1 (3)
                  [3]
               14 RETURN_VALUE                      0
                  []
    Successors:


    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["foo", "dummy.foo", n0, n0, n0, n0]
          TOPLEVEL[foo] <- n3
          return n0


      function dummy.foo(test):
        b0:
          n0 <- None
          n3 <- LOCAL[test]
          if n3 then jmp b1 else jmp b2

        b1:
          return 3

        b2:
          return 3 |}]
