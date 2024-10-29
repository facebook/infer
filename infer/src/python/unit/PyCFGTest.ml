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
               10 POP_JUMP_IF_FALSE                12 (to 24)
       5       12 LOAD_GLOBAL                       2 (print)
               14 LOAD_CONST                        2 ("case 1")
               16 CALL_FUNCTION                     1
               18 POP_TOP                           0
               20 LOAD_CONST                        0 (None)
               22 RETURN_VALUE                      0
       7 >>>   24 LOAD_CONST                        3 (0)
               26 STORE_FAST                        1 (i)
       8       28 LOAD_FAST                         1 (i)
               30 LOAD_FAST                         0 (N)
               32 COMPARE_OP                        0
               34 POP_JUMP_IF_FALSE                28 (to 56)
       9 >>>   36 LOAD_FAST                         1 (i)
               38 LOAD_CONST                        4 (1)
               40 BINARY_ADD                        0
               42 STORE_FAST                        1 (i)
       8       44 LOAD_FAST                         1 (i)
               46 LOAD_FAST                         0 (N)
               48 COMPARE_OP                        0
               50 POP_JUMP_IF_TRUE                 18 (to 36)
               52 LOAD_CONST                        0 (None)
               54 RETURN_VALUE                      0
         >>>   56 LOAD_CONST                        0 (None)
               58 RETURN_VALUE                      0
    CFG successors:
       0: 12 24
      12:
      24: 36 56
      36: 52 36
      52:
      56:
    CFG predecessors:
       0:
      12: 0
      24: 0
      36: 24 36
      52: 36
      56: 24
    topological order: 0 24 56 36 52 12

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
         >>>   10 FOR_ITER                         35 (to +70)
               12 STORE_NAME                        2 (i)
       4       14 LOAD_NAME                         3 (foo)
               16 LOAD_METHOD                       4 (Foo)
               18 CALL_METHOD                       0
               20 STORE_NAME                        5 (e)
       5       22 SETUP_FINALLY                    24
               24 SETUP_FINALLY                     6
       6       26 LOAD_NAME                         6 (print)
               28 LOAD_CONST                        0 ("yolo")
               30 CALL_FUNCTION                     1
               32 POP_TOP                           0
               34 POP_BLOCK                         0
               36 JUMP_FORWARD                      8 (to +16)
       7 >>>   38 POP_TOP                           0
               40 POP_TOP                           0
               42 POP_TOP                           0
       8       44 LOAD_NAME                         7 (do)
               46 CALL_FUNCTION                     0
               48 POP_TOP                           0
               50 POP_EXCEPT                        0
               52 JUMP_FORWARD                      3 (to +6)
      10 >>>   54 LOAD_NAME                         8 (other_thing)
               56 CALL_FUNCTION                     0
               58 POP_TOP                           0
         >>>   60 POP_BLOCK                         0
      12       62 LOAD_NAME                         5 (e)
               64 LOAD_METHOD                       9 (bar)
               66 CALL_METHOD                       0
               68 POP_TOP                           0
               70 JUMP_ABSOLUTE                     5 (to 10)
         >>>   72 LOAD_NAME                         5 (e)
               74 LOAD_METHOD                       9 (bar)
               76 CALL_METHOD                       0
               78 POP_TOP                           0
               80 RERAISE                           0
      13 >>>   82 LOAD_NAME                        10 (done)
               84 CALL_FUNCTION                     0
               86 POP_TOP                           0
               88 LOAD_CONST                        1 (None)
               90 RETURN_VALUE                      0
    CFG successors:
       0: 10
      10: 12 82(-2)
      12: 54
      38: 60
      54: 60
      60: 10
      72: 82
      82:
    CFG predecessors:
       0:
      10: 0 60
      12: 10
      38:
      54: 12
      60: 54
      72:
      82: 10
    topological order: 0 10 82 12 54 60 |}]
