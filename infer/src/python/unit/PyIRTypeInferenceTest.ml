(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let run module_ = PyIRTypeInference.gen_module_default_type_debug module_

let%expect_test _ =
  let source =
    {|
import dir.random
import dir.asyncio as a
from dir1.dir2.mod import x as y
from dir1.dir2 import mod

x = 0

def f(y, l):
    if y:
        g(0, y)
    else:
        for i in l:
            print(i)
        done()

async def g():
    await sleep(1)

@decofun
def h():
    pass

@decoclass
class D:
    def foo():
        pass

class C:
    def foo():
        pass
|}
  in
  PyIR.test ~run source ;
  [%expect
    {|
    type dummy = {
        dir: Import[dir.__init__]
        a: ImportFrom[dir.__init__, asyncio]
        y: ImportFrom[dir1.dir2.mod, x]
        mod: ImportFrom[dir1.dir2, mod]
        f: Closure[dummy.f]
        g: Closure[dummy.g]
        h: Closure[dummy.h]
        D: Class[D]
        C: Class[C]
    }

    type C = {
        foo: Closure[dummy.C.foo]
    }

    type D = {
        foo: Closure[dummy.D.foo]
    } |}]
