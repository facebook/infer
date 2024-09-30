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
N = 10
if x<y:
    print("case 1")
else:
    i=0
    while i<N:
        i = i+1
|}
  in
  PyIR.test_cfg_skeleton source ;
  [%expect
    {|
      2        0 LOAD_CONST    0
               2 STORE_NAME    0
      3        4 LOAD_NAME    1
               6 LOAD_NAME    2
               8 COMPARE_OP    0
              10 POP_JUMP_IF_FALSE   22
      4       12 LOAD_NAME    3
              14 LOAD_CONST    1
              16 CALL_FUNCTION    1
              18 POP_TOP    0
              20 JUMP_FORWARD   22
      6 >>>   22 LOAD_CONST    2
              24 STORE_NAME    4
      7 >>>   26 LOAD_NAME    4
              28 LOAD_NAME    0
              30 COMPARE_OP    0
              32 POP_JUMP_IF_FALSE   44
      8       34 LOAD_NAME    4
              36 LOAD_CONST    3
              38 BINARY_ADD    0
              40 STORE_NAME    4
              42 JUMP_ABSOLUTE   26
        >>>   44 LOAD_CONST    4
              46 RETURN_VALUE    0

     0: 12 22
    12: 44
    22: 26
    26: 34 44
    34: 26
    44: |}]


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
      2        0 LOAD_NAME    0
               2 CALL_FUNCTION    0
               4 POP_TOP    0
      3        6 LOAD_NAME    1
               8 GET_ITER    0
        >>>   10 FOR_ITER   66
              12 STORE_NAME    2
      4       14 LOAD_NAME    3
              16 LOAD_METHOD    4
              18 CALL_METHOD    0
              20 STORE_NAME    5
      5       22 SETUP_FINALLY   42
              24 SETUP_FINALLY   12
      6       26 LOAD_NAME    7
              28 LOAD_CONST    0
              30 CALL_FUNCTION    1
              32 POP_TOP    0
              34 POP_BLOCK    0
              36 JUMP_FORWARD   18
      7 >>>   38 POP_TOP    0
              40 POP_TOP    0
              42 POP_TOP    0
      8       44 LOAD_NAME    8
              46 CALL_FUNCTION    0
              48 POP_TOP    0
              50 POP_EXCEPT    0
              52 JUMP_FORWARD    8
              54 END_FINALLY    0
     10 >>>   56 LOAD_NAME    9
              58 CALL_FUNCTION    0
              60 POP_TOP    0
        >>>   62 POP_BLOCK    0
              64 BEGIN_FINALLY    0
     12 >>>   66 LOAD_NAME    5
              68 LOAD_METHOD    6
              70 CALL_METHOD    0
              72 POP_TOP    0
              74 END_FINALLY    0
              76 JUMP_ABSOLUTE   10
     13 >>>   78 LOAD_NAME   10
              80 CALL_FUNCTION    0
              82 POP_TOP    0
              84 LOAD_CONST    1
              86 RETURN_VALUE    0

     0: 10
    10: 12 78(-2)
    12: 56
    38: 62
    54: 56
    56: 62
    62: 66
    66: 10
    78: |}]
