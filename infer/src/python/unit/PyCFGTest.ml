(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let%expect_test _ =
  let source =
    {|
def main():
    N = 10
    if x<y:
        print("case 1")
    else:
        i=0
        while i<N:
            i = i+1

def foo():
      pass
|}
  in
  PyIR.test_cfg_skeleton source ;
  [%expect
    {|
    dummy
       2        0 LOAD_CONST                        0 (<code object main>)
                2 LOAD_CONST                        1 ("main")
                4 MAKE_FUNCTION                     0
                6 STORE_NAME                        0 (main)
      11        8 LOAD_CONST                        2 (<code object foo>)
               10 LOAD_CONST                        3 ("foo")
               12 MAKE_FUNCTION                     0
               14 STORE_NAME                        1 (foo)
               16 LOAD_CONST                        4 (None)
               18 RETURN_VALUE                      0
    CFG successors:
       0:
    CFG predecessors:
       0:
    topological order: 0

    dummy.main
       3        0 LOAD_CONST                        1 (10)
                2 STORE_FAST                        0 (N)
       4        4 LOAD_GLOBAL                       0 (x)
                6 LOAD_GLOBAL                       1 (y)
                8 COMPARE_OP                        0
               10 POP_JUMP_IF_FALSE                22 (to 22)
       5       12 LOAD_GLOBAL                       2 (print)
               14 LOAD_CONST                        2 ("case 1")
               16 CALL_FUNCTION                     1
               18 POP_TOP                           0
               20 JUMP_FORWARD                     22 (to +22)
       7 >>>   22 LOAD_CONST                        3 (0)
               24 STORE_FAST                        1 (i)
       8 >>>   26 LOAD_FAST                         1 (i)
               28 LOAD_FAST                         0 (N)
               30 COMPARE_OP                        0
               32 POP_JUMP_IF_FALSE                44 (to 44)
       9       34 LOAD_FAST                         1 (i)
               36 LOAD_CONST                        4 (1)
               38 BINARY_ADD                        0
               40 STORE_FAST                        1 (i)
               42 JUMP_ABSOLUTE                    26 (to 26)
         >>>   44 LOAD_CONST                        0 (None)
               46 RETURN_VALUE                      0
    CFG successors:
       0: 12 22
      12: 44
      22: 26
      26: 34 44
      34: 26
      44:
    CFG predecessors:
       0:
      12: 0
      22: 0
      26: 22 34
      34: 26
      44: 26 12
    topological order: 0 22 26 34 12 44

    dummy.foo
      12        0 LOAD_CONST                        0 (None)
                2 RETURN_VALUE                      0
    CFG successors:
       0:
    CFG predecessors:
       0:
    topological order: 0 |}]


let%expect_test _ =
  let source =
    {|
start()
for i in x:
    e = foo.Foo()
    try:
        print("yolo")
    except:
        do()
    else:
        other_thing()
    finally:
        e.bar()
done()
|}
  in
  PyIR.test_cfg_skeleton source ;
  [%expect
    {|
    dummy
       2        0 LOAD_NAME                         0 (start)
                2 CALL_FUNCTION                     0
                4 POP_TOP                           0
       3        6 LOAD_NAME                         1 (x)
                8 GET_ITER                          0
         >>>   10 FOR_ITER                         66 (to +66)
               12 STORE_NAME                        2 (i)
       4       14 LOAD_NAME                         3 (foo)
               16 LOAD_METHOD                       4 (Foo)
               18 CALL_METHOD                       0
               20 STORE_NAME                        5 (e)
       5       22 SETUP_FINALLY                    42
               24 SETUP_FINALLY                    12
       6       26 LOAD_NAME                         7 (print)
               28 LOAD_CONST                        0 ("yolo")
               30 CALL_FUNCTION                     1
               32 POP_TOP                           0
               34 POP_BLOCK                         0
               36 JUMP_FORWARD                     18 (to +18)
       7 >>>   38 POP_TOP                           0
               40 POP_TOP                           0
               42 POP_TOP                           0
       8       44 LOAD_NAME                         8 (do)
               46 CALL_FUNCTION                     0
               48 POP_TOP                           0
               50 POP_EXCEPT                        0
               52 JUMP_FORWARD                      8 (to +8)
               54 END_FINALLY                       0
      10 >>>   56 LOAD_NAME                         9 (other_thing)
               58 CALL_FUNCTION                     0
               60 POP_TOP                           0
         >>>   62 POP_BLOCK                         0
               64 BEGIN_FINALLY                     0
      12 >>>   66 LOAD_NAME                         5 (e)
               68 LOAD_METHOD                       6 (bar)
               70 CALL_METHOD                       0
               72 POP_TOP                           0
               74 END_FINALLY                       0
               76 JUMP_ABSOLUTE                    10 (to 10)
      13 >>>   78 LOAD_NAME                        10 (done)
               80 CALL_FUNCTION                     0
               82 POP_TOP                           0
               84 LOAD_CONST                        1 (None)
               86 RETURN_VALUE                      0
    CFG successors:
       0: 10
      10: 12 78(-2)
      12: 56
      38: 62
      54: 56
      56: 62
      62: 66
      66: 76
      76: 10
      78:
    CFG predecessors:
       0:
      10: 0 76
      12: 10
      38:
      54:
      56: 12
      62: 56
      66: 62
      76: 66
      78: 10
    topological order: 0 10 78 12 56 62 66 76 |}]
