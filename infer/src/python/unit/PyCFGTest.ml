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
               10 POP_JUMP_IF_FALSE                12 (to 12)
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
               34 POP_JUMP_IF_FALSE                28 (to 28)
       9 >>>   36 LOAD_FAST                         1 (i)
               38 LOAD_CONST                        4 (1)
               40 BINARY_ADD                        0
               42 STORE_FAST                        1 (i)
       8       44 LOAD_FAST                         1 (i)
               46 LOAD_FAST                         0 (N)
               48 COMPARE_OP                        0
               50 POP_JUMP_IF_TRUE                 18 (to 18)
               52 LOAD_CONST                        0 (None)
               54 RETURN_VALUE                      0
         >>>   56 LOAD_CONST                        0 (None)
               58 RETURN_VALUE                      0
    CFG successors:
       0: 12 12
      12:
      24: 36 28
      36: 52 18
      52:
      56:
    CFG predecessors:
       0:
      12: 0 0
      24:
      36:
      52:
      56:
    topological order: 0 12

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
  [%expect {|
    IR error: Unsupported opcode: RERAISE |}]
