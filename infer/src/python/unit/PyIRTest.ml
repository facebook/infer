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
# user-defined top level function
def my_fun(x, y):
        print(x)
        print(y)
        # local variable z
        z = x + y
        return z

a = 10
# global variable z
z = my_fun(42, a)
print(z)
      |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["my_fun", "dummy.my_fun", None, None, None, None]
          TOPLEVEL[my_fun] <- n0
          TOPLEVEL[a] <- 10
          n1 <- TOPLEVEL[my_fun]
          n2 <- TOPLEVEL[a]
          n3 <- $Call(n1, 42, n2, None)
          TOPLEVEL[z] <- n3
          n4 <- TOPLEVEL[print]
          n5 <- TOPLEVEL[z]
          n6 <- $Call(n4, n5, None)
          return None


      function dummy.my_fun(x, y):
        b0:
          n0 <- GLOBAL[print]
          n1 <- LOCAL[x]
          n2 <- $Call(n0, n1, None)
          n3 <- GLOBAL[print]
          n4 <- LOCAL[y]
          n5 <- $Call(n3, n4, None)
          n6 <- LOCAL[x]
          n7 <- LOCAL[y]
          n8 <- $Binary.Add(n6, n7, None)
          LOCAL[z] <- n8
          n9 <- LOCAL[z]
          return n9 |}]


let%expect_test _ =
  let source =
    {|
# testing global python attribute
def update_global():
        global z
        z = z + 1

z = 0
update_global()
print(z)
      |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["update_global", "dummy.update_global", None, None, None, None]
          TOPLEVEL[update_global] <- n0
          GLOBAL[z] <- 0
          n1 <- TOPLEVEL[update_global]
          n2 <- $Call(n1, None)
          n3 <- TOPLEVEL[print]
          n4 <- GLOBAL[z]
          n5 <- $Call(n3, n4, None)
          return None


      function dummy.update_global():
        b0:
          n0 <- GLOBAL[z]
          n1 <- $Binary.Add(n0, 1, None)
          GLOBAL[z] <- n1
          return None |}]


let%expect_test _ =
  let source =
    {|
def coin():
    return False

def f(x, y):
    if coin():
          return x
    else:
          return y
      |}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Jump to next instruction detected, but next instruction is missing |}]


let%expect_test _ =
  let source =
    {|
def coin():
    return False

def f(x, y):
    z = 0
    if coin():
          z = x
    else:
          z = y
    return z
      |}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Jump to next instruction detected, but next instruction is missing |}]


let%expect_test _ =
  let source =
    {|
def coin():
    return False

def f(x, y):
    z = 0
    if coin():
          if coin():
            z = x
          else:
            return 1664
          z = z + 1
    else:
          z = z + 1
          if coin():
            return 42
          else:
            z = y
    return z
      |}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Jump to next instruction detected, but next instruction is missing |}]


let%expect_test _ =
  let source = {|
def foo(x):
    pass

def f(x):
    foo(1 if x else 0)
      |} in
  PyIR.test source ;
  [%expect {|
    IR error: Jump to next instruction detected, but next instruction is missing |}]


let%expect_test _ =
  let source = {|
for x in range(10):
    print(x)
      |} in
  PyIR.test source ;
  [%expect {|
    IR error: Jump to next instruction detected, but next instruction is missing |}]


let%expect_test _ =
  let source =
    {|
def f(x, y, l, bar, toto):
    for x in l:
        with bar(), toto() as obj:
            if y:
                continue
            print('nop')
        |}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Unsupported opcode: NOP |}]


let%expect_test _ =
  let source =
    {|
print(42)

def print(x):
        return x

print(42)

def f(x):
        print(x)
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- TOPLEVEL[print]
          n1 <- $Call(n0, 42, None)
          n2 <- $MakeFunction["print", "dummy.print", None, None, None, None]
          TOPLEVEL[print] <- n2
          n3 <- TOPLEVEL[print]
          n4 <- $Call(n3, 42, None)
          n5 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n5
          return None


      function dummy.f(x):
        b0:
          n0 <- GLOBAL[print]
          n1 <- LOCAL[x]
          n2 <- $Call(n0, n1, None)
          return None


      function dummy.print(x):
        b0:
          n0 <- LOCAL[x]
          return n0 |}]


let%expect_test _ =
  let source =
    {|
def f0(x: int, y, z:float):
        pass

def f1(x, y:str) -> bool:
        pass
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- TOPLEVEL[int]
          n1 <- TOPLEVEL[float]
          n2 <- $MakeFunction["f0", "dummy.f0", None, None, $BuildTuple("x", n0, "z", n1), None]
          TOPLEVEL[f0] <- n2
          n3 <- TOPLEVEL[str]
          n4 <- TOPLEVEL[bool]
          n5 <- $MakeFunction["f1", "dummy.f1", None, None, $BuildTuple("y", n3, "return", n4), None]
          TOPLEVEL[f1] <- n5
          return None


      function dummy.f0(x, y, z):
        b0:
          return None


      function dummy.f1(x, y):
        b0:
          return None |}]


let%expect_test _ =
  let source =
    {|
def expect_int(x: int):
        pass

def get() -> int:
        return 42

expect_int(get())
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- TOPLEVEL[int]
          n1 <- $MakeFunction["expect_int", "dummy.expect_int", None, None, $BuildTuple("x", n0), None]
          TOPLEVEL[expect_int] <- n1
          n2 <- TOPLEVEL[int]
          n3 <- $MakeFunction["get", "dummy.get", None, None, $BuildTuple("return", n2), None]
          TOPLEVEL[get] <- n3
          n4 <- TOPLEVEL[expect_int]
          n5 <- TOPLEVEL[get]
          n6 <- $Call(n5, None)
          n7 <- $Call(n4, n6, None)
          return None


      function dummy.expect_int(x):
        b0:
          return None


      function dummy.get():
        b0:
          return 42 |}]


let%expect_test _ =
  let source =
    {|
def expect(x: object) -> None:
        pass

def get() -> int:
        return 42

expect(get())
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- TOPLEVEL[object]
          n1 <- $MakeFunction["expect", "dummy.expect", None, None, $BuildTuple("x", n0, "return", None), None]
          TOPLEVEL[expect] <- n1
          n2 <- TOPLEVEL[int]
          n3 <- $MakeFunction["get", "dummy.get", None, None, $BuildTuple("return", n2), None]
          TOPLEVEL[get] <- n3
          n4 <- TOPLEVEL[expect]
          n5 <- TOPLEVEL[get]
          n6 <- $Call(n5, None)
          n7 <- $Call(n4, n6, None)
          return None


      function dummy.expect(x):
        b0:
          return None


      function dummy.get():
        b0:
          return 42 |}]


let%expect_test _ =
  let source = {|
def f(x, y):
  return (x == y)
        |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          return None


      function dummy.f(x, y):
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[y]
          n2 <- $Compare.eq(n0, n1, None)
          return n2 |}]


let%expect_test _ =
  let source = "True != False" in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $Compare.neq(true, false, None)
          return None |}]


let%expect_test _ =
  let source = {|
def f(x, y, z, t):
        return (x and y) or (z and t)
        |} in
  PyIR.test source ;
  [%expect {|
    IR error: Jump to next instruction detected, but next instruction is missing |}]


let%expect_test _ =
  let source = {|
def f(x, y):
  return (x > y)
        |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          return None


      function dummy.f(x, y):
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[y]
          n2 <- $Compare.gt(n0, n1, None)
          return n2 |}]


let%expect_test _ =
  let source = {|
def f(x, y):
  return (x <= y)
        |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          return None


      function dummy.f(x, y):
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[y]
          n2 <- $Compare.le(n0, n1, None)
          return n2 |}]


let%expect_test _ =
  let source =
    {|
def is_check(x):
          return x is None

def is_not_check(x):
          return x is not None

def in_check(x, l):
          return x in l

def in_not_check(x, l):
          return not (x in l)
          |}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Unsupported opcode: IS_OP |}]


let%expect_test _ =
  let source =
    {|
t = (1, 2, 3) # will be a constant, not a BUILD_TUPLE
def f(x, y, z):
        return (x, y, z) # should be BUILD_TUPLE
|}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          TOPLEVEL[t] <- $BuildTuple(1, 2, 3)
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          return None


      function dummy.f(x, y, z):
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[y]
          n2 <- LOCAL[z]
          return $BuildTuple(n0, n1, n2) |}]


let%expect_test _ =
  let source =
    {|
l = [1, 2, 3]
print(l)

def build_list():
          return [1, 2, 3]

[x, y, z] = build_list()
|}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Unsupported opcode: LIST_EXTEND |}]


let%expect_test _ =
  let source =
    {|
def f(foo, bar):
    with foo() as foo0:
        with bar() as bar0:
            print(bar0)
        print(foo0)

        return 42
        |}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Unsupported opcode: WITH_EXCEPT_START |}]


let%expect_test _ =
  let source = {|
def f():
        pass

(a, b) = f()
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          n1 <- TOPLEVEL[f]
          n2 <- $Call(n1, None)
          TOPLEVEL[a] <- n2[0]
          TOPLEVEL[b] <- n2[1]
          return None


      function dummy.f():
        b0:
          return None |}]


let%expect_test _ =
  let source = {|
def f(z, x, y):
        pass

f(0, y=2, x=1)
        |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          n1 <- TOPLEVEL[f]
          n2 <- $Call(n1, 0, 2, 1, $BuildTuple("y", "x"))
          return None


      function dummy.f(z, x, y):
        b0:
          return None |}]


let%expect_test _ =
  let source =
    {|
def f(m, a, b, c):
    while (a, b) not in m:
        b -= 1
    while (a, c) not in m:
        c += 1
  |}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Unsupported opcode: CONTAINS_OP |}]


let%expect_test _ =
  let source =
    {|
def f(name, args):
    return f"foo.{name!r}{name!s}{name!a}"

def test_arguments(x, y, width):
    return f'x={x*y:{width}}'
          |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          n1 <- $MakeFunction["test_arguments", "dummy.test_arguments", None, None, None, None]
          TOPLEVEL[test_arguments] <- n1
          return None


      function dummy.f(name, args):
        b0:
          n0 <- LOCAL[name]
          n1 <- $FormatFn.repr(n0, None)
          n2 <- $Format(n1, None, None)
          n3 <- LOCAL[name]
          n4 <- $FormatFn.str(n3, None)
          n5 <- $Format(n4, None, None)
          n6 <- LOCAL[name]
          n7 <- $FormatFn.ascii(n6, None)
          n8 <- $Format(n7, None, None)
          return $BuildString("foo.", n2, n5, n8)


      function dummy.test_arguments(x, y, width):
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[y]
          n2 <- $Binary.Multiply(n0, n1, None)
          n3 <- LOCAL[width]
          n4 <- $Format(n3, None, None)
          n5 <- $Format(n2, n4, None)
          return $BuildString("x=", n5) |}]


let%expect_test _ =
  let source =
    {|
def pos(x):
        return +x

def neg(x):
        return -x

def test_not(x):
        return not x

def inv(x):
        return ~x
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["pos", "dummy.pos", None, None, None, None]
          TOPLEVEL[pos] <- n0
          n1 <- $MakeFunction["neg", "dummy.neg", None, None, None, None]
          TOPLEVEL[neg] <- n1
          n2 <- $MakeFunction["test_not", "dummy.test_not", None, None, None, None]
          TOPLEVEL[test_not] <- n2
          n3 <- $MakeFunction["inv", "dummy.inv", None, None, None, None]
          TOPLEVEL[inv] <- n3
          return None


      function dummy.inv(x):
        b0:
          n0 <- LOCAL[x]
          n1 <- $Unary.Invert(n0, None)
          return n1


      function dummy.neg(x):
        b0:
          n0 <- LOCAL[x]
          n1 <- $Unary.Negative(n0, None)
          return n1


      function dummy.pos(x):
        b0:
          n0 <- LOCAL[x]
          n1 <- $Unary.Positive(n0, None)
          return n1


      function dummy.test_not(x):
        b0:
          n0 <- LOCAL[x]
          n1 <- $Unary.Not(n0, None)
          return n1 |}]


let%expect_test _ =
  let source =
    {|
gx = 100
def f(ax):
    lx = 1000

    def inner():
        ix = 20
        global gx
        nonlocal lx
        print(gx) # prints 100
        print(ax) # prints 42
        print(lx) # prints 1664
        print(ix) # 20
        gx = 10
        lx = 2
        return lx

    lx = 1664
    return inner

g = f(42)
print(g()) # prints 2
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          GLOBAL[gx] <- 100
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          n1 <- TOPLEVEL[f]
          n2 <- $Call(n1, 42, None)
          TOPLEVEL[g] <- n2
          n3 <- TOPLEVEL[print]
          n4 <- TOPLEVEL[g]
          n5 <- $Call(n4, None)
          n6 <- $Call(n3, n5, None)
          return None


      function dummy.f(ax):
        b0:
          $StoreDeref(1,"lx", 1000)
          n0 <- $LoadClosure(0,"ax")
          n1 <- $LoadClosure(1,"lx")
          n2 <- $MakeFunction["inner", "dummy.f.inner", None, None, None, $BuildTuple(n0, n1)]
          LOCAL[inner] <- n2
          $StoreDeref(1,"lx", 1664)
          n3 <- LOCAL[inner]
          return n3


      function dummy.f.inner(ix):
        b0:
          LOCAL[ix] <- 20
          n0 <- GLOBAL[print]
          n1 <- GLOBAL[gx]
          n2 <- $Call(n0, n1, None)
          n3 <- GLOBAL[print]
          n4 <- $LoadDeref(0,"ax")
          n5 <- $Call(n3, n4, None)
          n6 <- GLOBAL[print]
          n7 <- $LoadDeref(1,"lx")
          n8 <- $Call(n6, n7, None)
          n9 <- GLOBAL[print]
          n10 <- LOCAL[ix]
          n11 <- $Call(n9, n10, None)
          GLOBAL[gx] <- 10
          $StoreDeref(1,"lx", 2)
          n12 <- $LoadDeref(1,"lx")
          return n12 |}]


let%expect_test _ =
  let source =
    {|
g = [x + 1 for x in l]
g0 = [x + 2 for x in l]
print(g)
print(g0)

def f(l):
   r = [x + 1 for x in l]
   r0 = [x + 2 for x in l]
   print(r)
   print(r0)
        |}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Jump to next instruction detected, but next instruction is missing |}]


let%expect_test _ =
  let source =
    {|
def f(l):
  r = {x + 1 for x in l }
  return r


def g(l):
  squared_dict = {num: num ** 2 for num in l}
  return r
          |}
  in
  PyIR.test source ;
  [%expect
    {xxx|
    IR error: Jump to next instruction detected, but next instruction is missing |xxx}]


let%expect_test _ =
  let source =
    {|
async def f():
  return True

async def g():
  if await f():
    print(0)
  else:
    print(1)
          |}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Unsupported opcode: GEN_START |}]


let%expect_test _ =
  let source = {|
def m(self, x, y, test):
    return foo(self, x if test else y)
|} in
  PyIR.test source ;
  [%expect {|
    IR error: Jump to next instruction detected, but next instruction is missing |}]


let%expect_test _ =
  let source = {|
def m(self, x, y, test):
    return self.foo(x if test else y)
|} in
  PyIR.test source ;
  [%expect {|
    IR error: Jump to next instruction detected, but next instruction is missing |}]


let%expect_test _ =
  let source = {|
def m(x, y, test):
    return (x if test else y).foo()
|} in
  PyIR.test source ;
  [%expect {|
    IR error: Jump to next instruction detected, but next instruction is missing |}]


let%expect_test _ =
  let source =
    {|
class C:
    def foo(self):
        print('I am foo')

o = C()
o.foo()
o.foo = lambda : print('I am not foo')
o.foo()
#I am foo
#I am not foo
|}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- $MakeFunction["C", "dummy.C", None, None, None, None]
          n1 <- $BuildClass(n0, "C", None)
          TOPLEVEL[C] <- n1
          n2 <- TOPLEVEL[C]
          n3 <- $Call(n2, None)
          TOPLEVEL[o] <- n3
          n4 <- TOPLEVEL[o]
          n5 <- $CallMethod[foo](n4, None)
          n6 <- $MakeFunction["_$lambda", "dummy._$lambda", None, None, None, None]
          n7 <- TOPLEVEL[o]
          n7.foo <- n6
          n8 <- TOPLEVEL[o]
          n9 <- $CallMethod[foo](n8, None)
          return None


      function dummy.C():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          n1 <- $MakeFunction["foo", "dummy.C.foo", None, None, None, None]
          TOPLEVEL[foo] <- n1
          return None


      function dummy._$lambda():
        b0:
          n0 <- GLOBAL[print]
          n1 <- $Call(n0, "I am not foo", None)
          return n1


      function dummy.C.foo(self):
        b0:
          n0 <- GLOBAL[print]
          n1 <- $Call(n0, "I am foo", None)
          return None |}]


let%expect_test _ =
  let source = {|
res = dict.attr(0 if not False else 1)
|} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- TOPLEVEL[dict]
          n1 <- $CallMethod[attr](n0, 0, None)
          TOPLEVEL[res] <- n1
          return None |}]


let%expect_test _ =
  let source =
    {|
async def foo():
    for i in range(num):
        async with await read() as f:
            return
|}
  in
  PyIR.test source ;
  [%expect {|
    IR error: Unsupported opcode: GEN_START |}]
