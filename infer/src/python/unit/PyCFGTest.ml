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
  [%expect {| |}]


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
  [%expect {| |}]
