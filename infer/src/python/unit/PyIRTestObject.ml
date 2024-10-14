(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

(* Tests with classes *)

let%expect_test _ =
  let source =
    {|
class C:
        def __init__(self, x, y):
            self.x = x
            self.y = y

        def get(self):
            return self.x

        def set(self, x):
            self.x = x

c = C(0, "a")
c.x
c.get()
c.set(42)
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), "C", None)
          TOPLEVEL[C] <- n0
          n1 <- TOPLEVEL[C]
          n2 <- $Call(n1, 0, "a", None)
          TOPLEVEL[c] <- n2
          n3 <- TOPLEVEL[c]
          n4 <- n3.x
          n5 <- TOPLEVEL[c]
          n6 <- $CallMethod[get](n5, None)
          n7 <- TOPLEVEL[c]
          n8 <- $CallMethod[set](n7, 42, None)
          return None


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.C.__init__, {})
          TOPLEVEL[get] <- $FuncObj(get, dummy.C.get, {})
          TOPLEVEL[set] <- $FuncObj(set, dummy.C.set, {})
          return None


      dummy.C.__init__:
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[self]
          n1.x <- n0
          n2 <- LOCAL[y]
          n3 <- LOCAL[self]
          n3.y <- n2
          return None


      dummy.C.get:
        b0:
          n0 <- LOCAL[self]
          n1 <- n0.x
          return n1


      dummy.C.set:
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[self]
          n1.x <- n0
          return None |}]


let%expect_test _ =
  let source =
    {|
class IntBox:
        x: int
#        f: Callable[[int, bool, str], None]

        def __init__(self, x: int) -> None:
            self.x = x
#            self.f = lambda i: lambda b: lambda s: print(42)

        def get(self) -> int:
            return self.x

        def set(self, x: int) -> None:
            self.x = x

        def run(self) -> None:
#            self.f(3)(False)("yolo")
            pass

        # Stupid function to PyIR.test the staticmethod decorator + type annotations
        @staticmethod
        def id(x: int) -> int:
          return x

def getX(box: IntBox) -> int:
          return box.get()

c = IntBox(10)
c.x
c.z = 10
c.get()
c.set(42)
c.run()
print(c.z)
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $BuildClass($FuncObj(IntBox, dummy.IntBox, {}), "IntBox", None)
          TOPLEVEL[IntBox] <- n0
          n1 <- TOPLEVEL[IntBox]
          n2 <- TOPLEVEL[int]
          TOPLEVEL[getX] <- $FuncObj(getX, dummy.getX, {})
          n3 <- TOPLEVEL[IntBox]
          n4 <- $Call(n3, 10, None)
          TOPLEVEL[c] <- n4
          n5 <- TOPLEVEL[c]
          n6 <- n5.x
          n7 <- TOPLEVEL[c]
          n7.z <- 10
          n8 <- TOPLEVEL[c]
          n9 <- $CallMethod[get](n8, None)
          n10 <- TOPLEVEL[c]
          n11 <- $CallMethod[set](n10, 42, None)
          n12 <- TOPLEVEL[c]
          n13 <- $CallMethod[run](n12, None)
          n14 <- TOPLEVEL[print]
          n15 <- TOPLEVEL[c]
          n16 <- n15.z
          n17 <- $Call(n14, n16, None)
          return None


      dummy.IntBox:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "IntBox"
          $SETUP_ANNOTATIONS
          n1 <- TOPLEVEL[int]
          n2 <- TOPLEVEL[__annotations__]
          n2["x"] <- n1
          n3 <- TOPLEVEL[int]
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.IntBox.__init__, {})
          n4 <- TOPLEVEL[int]
          TOPLEVEL[get] <- $FuncObj(get, dummy.IntBox.get, {})
          n5 <- TOPLEVEL[int]
          TOPLEVEL[set] <- $FuncObj(set, dummy.IntBox.set, {})
          TOPLEVEL[run] <- $FuncObj(run, dummy.IntBox.run, {})
          n6 <- TOPLEVEL[staticmethod]
          n7 <- TOPLEVEL[int]
          n8 <- TOPLEVEL[int]
          n9 <- $Call(n6, $FuncObj(id, dummy.IntBox.id, {}), None)
          TOPLEVEL[id] <- n9
          return None


      dummy.IntBox.__init__:
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[self]
          n1.x <- n0
          return None


      dummy.IntBox.get:
        b0:
          n0 <- LOCAL[self]
          n1 <- n0.x
          return n1


      dummy.getX:
        b0:
          n0 <- LOCAL[box]
          n1 <- $CallMethod[get](n0, None)
          return n1


      dummy.IntBox.id:
        b0:
          n0 <- LOCAL[x]
          return n0


      dummy.IntBox.run:
        b0:
          return None


      dummy.IntBox.set:
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[self]
          n1.x <- n0
          return None |}]


let%expect_test _ =
  let source =
    {|
class C:
    @staticmethod
    def f():
          pass

    @staticmethod
    def typed_f(x:int) -> int:
          return x

class D(C):
    pass
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), "C", None)
          TOPLEVEL[C] <- n0
          n1 <- TOPLEVEL[C]
          n2 <- $BuildClass($FuncObj(D, dummy.D, {}), "D", n1, None)
          TOPLEVEL[D] <- n2
          return None


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          n1 <- TOPLEVEL[staticmethod]
          n2 <- $Call(n1, $FuncObj(f, dummy.C.f, {}), None)
          TOPLEVEL[f] <- n2
          n3 <- TOPLEVEL[staticmethod]
          n4 <- TOPLEVEL[int]
          n5 <- TOPLEVEL[int]
          n6 <- $Call(n3, $FuncObj(typed_f, dummy.C.typed_f, {}), None)
          TOPLEVEL[typed_f] <- n6
          return None


      dummy.D:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "D"
          return None


      dummy.C.f:
        b0:
          return None


      dummy.C.typed_f:
        b0:
          n0 <- LOCAL[x]
          return n0 |}]


let%expect_test _ =
  let source = {|
class C:
    @staticmethod
    def f():
          pass

C.f()
        |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), "C", None)
          TOPLEVEL[C] <- n0
          n1 <- TOPLEVEL[C]
          n2 <- $CallMethod[f](n1, None)
          return None


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          n1 <- TOPLEVEL[staticmethod]
          n2 <- $Call(n1, $FuncObj(f, dummy.C.f, {}), None)
          TOPLEVEL[f] <- n2
          return None


      dummy.C.f:
        b0:
          return None |}]


let%expect_test _ =
  let source =
    {|
class A:
    def f(self):
        pass

class C:
    a: A

def g(c: C) -> None:
    print(c.a.f())

        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $BuildClass($FuncObj(A, dummy.A, {}), "A", None)
          TOPLEVEL[A] <- n0
          n1 <- $BuildClass($FuncObj(C, dummy.C, {}), "C", None)
          TOPLEVEL[C] <- n1
          n2 <- TOPLEVEL[C]
          TOPLEVEL[g] <- $FuncObj(g, dummy.g, {})
          return None


      dummy.A:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "A"
          TOPLEVEL[f] <- $FuncObj(f, dummy.A.f, {})
          return None


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          $SETUP_ANNOTATIONS
          n1 <- TOPLEVEL[A]
          n2 <- TOPLEVEL[__annotations__]
          n2["a"] <- n1
          return None


      dummy.A.f:
        b0:
          return None


      dummy.g:
        b0:
          n0 <- GLOBAL[print]
          n1 <- LOCAL[c]
          n2 <- n1.a
          n3 <- $CallMethod[f](n2, None)
          n4 <- $Call(n0, n3, None)
          return None |}]


let%expect_test _ =
  let source =
    {|
class A:
        pass

class B:
        pass

class C(A, B):
        pass
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $BuildClass($FuncObj(A, dummy.A, {}), "A", None)
          TOPLEVEL[A] <- n0
          n1 <- $BuildClass($FuncObj(B, dummy.B, {}), "B", None)
          TOPLEVEL[B] <- n1
          n2 <- TOPLEVEL[A]
          n3 <- TOPLEVEL[B]
          n4 <- $BuildClass($FuncObj(C, dummy.C, {}), "C", n2, n3, None)
          TOPLEVEL[C] <- n4
          return None


      dummy.A:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "A"
          return None


      dummy.B:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "B"
          return None


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          return None |}]


let%expect_test _ =
  let source =
    {|
class C:
          def __init__(self):
            self.x = 0
def build():
          return [ C() ]

cs = build()

cs[0].x

          |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), "C", None)
          TOPLEVEL[C] <- n0
          TOPLEVEL[build] <- $FuncObj(build, dummy.build, {})
          n1 <- TOPLEVEL[build]
          n2 <- $Call(n1, None)
          TOPLEVEL[cs] <- n2
          n3 <- TOPLEVEL[cs]
          n4 <- n3[0]
          n5 <- n4.x
          return None


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.C.__init__, {})
          return None


      dummy.C.__init__:
        b0:
          n0 <- LOCAL[self]
          n0.x <- 0
          return None


      dummy.build:
        b0:
          n0 <- GLOBAL[C]
          n1 <- $Call(n0, None)
          return (unpacked)[n1] |}]


let%expect_test _ =
  let source =
    {|
def f():
  # BEHOLD a nested class
  class A:
    def __init__(self):
      self.x = 0
    def get(self):
      return self.x
  a = A()
  return a.get()

f()
          |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          n0 <- TOPLEVEL[f]
          n1 <- $Call(n0, None)
          return None


      dummy.f.A:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "f.<locals>.A"
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.f.A.__init__, {})
          TOPLEVEL[get] <- $FuncObj(get, dummy.f.A.get, {})
          return None


      dummy.f.A.__init__:
        b0:
          n0 <- LOCAL[self]
          n0.x <- 0
          return None


      dummy.f:
        b0:
          n0 <- $BuildClass($FuncObj(A, dummy.f.A, {}), "A", None)
          LOCAL[A] <- n0
          n1 <- LOCAL[A]
          n2 <- $Call(n1, None)
          LOCAL[a] <- n2
          n3 <- LOCAL[a]
          n4 <- $CallMethod[get](n3, None)
          return n4


      dummy.f.A.get:
        b0:
          n0 <- LOCAL[self]
          n1 <- n0.x
          return n1 |}]


let%expect_test _ =
  let source = {|
class C:
  pass

class D(C):
  pass
  |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), "C", None)
          TOPLEVEL[C] <- n0
          n1 <- TOPLEVEL[C]
          n2 <- $BuildClass($FuncObj(D, dummy.D, {}), "D", n1, None)
          TOPLEVEL[D] <- n2
          return None


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          return None


      dummy.D:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "D"
          return None |}]


let%expect_test _ =
  let source =
    {|
class C:
  pass

class D(C):
        def __init__(self):
          super().__init__()

class C0:
          def __init__(foo, x):
            foo.x = x

class D0(C0):
        def __init__(bar):
          super().__init__(42)
  |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), "C", None)
          TOPLEVEL[C] <- n0
          n1 <- TOPLEVEL[C]
          n2 <- $BuildClass($FuncObj(D, dummy.D, {}), "D", n1, None)
          TOPLEVEL[D] <- n2
          n3 <- $BuildClass($FuncObj(C0, dummy.C0, {}), "C0", None)
          TOPLEVEL[C0] <- n3
          n4 <- TOPLEVEL[C0]
          n5 <- $BuildClass($FuncObj(D0, dummy.D0, {}), "D0", n4, None)
          TOPLEVEL[D0] <- n5
          return None


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          return None


      dummy.C0:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C0"
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.C0.__init__, {})
          return None


      dummy.D:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "D"
          n1 <- $LoadClosure[0,"__class__"](None)
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.D.__init__, {})
          n2 <- $LoadClosure[0,"__class__"](None)
          TOPLEVEL[__classcell__] <- n2
          return n2


      dummy.D0:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "D0"
          n1 <- $LoadClosure[0,"__class__"](None)
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.D0.__init__, {})
          n2 <- $LoadClosure[0,"__class__"](None)
          TOPLEVEL[__classcell__] <- n2
          return n2


      dummy.C0.__init__:
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[foo]
          n1.x <- n0
          return None


      dummy.D.__init__:
        b0:
          n0 <- GLOBAL[super]
          n1 <- $Call(n0, None)
          n2 <- $CallMethod[__init__](n1, None)
          return None


      dummy.D0.__init__:
        b0:
          n0 <- GLOBAL[super]
          n1 <- $Call(n0, None)
          n2 <- $CallMethod[__init__](n1, 42, None)
          return None |}]


let%expect_test _ =
  let source =
    {|
import foo

class C(foo.D):
        def __init__(self, x):
          super().__init__(x)
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $ImportName(foo)(None, 0, None)
          TOPLEVEL[foo] <- n0
          n1 <- TOPLEVEL[foo]
          n2 <- n1.D
          n3 <- $BuildClass($FuncObj(C, dummy.C, {}), "C", n2, None)
          TOPLEVEL[C] <- n3
          return None


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          n1 <- $LoadClosure[0,"__class__"](None)
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.C.__init__, {})
          n2 <- $LoadClosure[0,"__class__"](None)
          TOPLEVEL[__classcell__] <- n2
          return n2


      dummy.C.__init__:
        b0:
          n0 <- GLOBAL[super]
          n1 <- $Call(n0, None)
          n2 <- LOCAL[x]
          n3 <- $CallMethod[__init__](n1, n2, None)
          return None |}]


let%expect_test _ =
  let source =
    {|
from abc import ABC, abstractmethod

class C(ABC):
    @abstractmethod
    def get(self) -> None:
      ...

    @abstractmethod
    @staticmethod
    def get_static0() -> None:
      ...

    @staticmethod
    @abstractmethod
    def get_static1() -> None:
      ...
|}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $ImportName(abc)(("ABC","abstractmethod"), 0, None)
          n1 <- $ImportFrom(ABC)(n0, None)
          TOPLEVEL[ABC] <- n1
          n2 <- $ImportFrom(abstractmethod)(n0, None)
          TOPLEVEL[abstractmethod] <- n2
          n3 <- TOPLEVEL[ABC]
          n4 <- $BuildClass($FuncObj(C, dummy.C, {}), "C", n3, None)
          TOPLEVEL[C] <- n4
          return None


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          n1 <- TOPLEVEL[abstractmethod]
          n2 <- $Call(n1, $FuncObj(get, dummy.C.get, {}), None)
          TOPLEVEL[get] <- n2
          n3 <- TOPLEVEL[abstractmethod]
          n4 <- TOPLEVEL[staticmethod]
          n5 <- $Call(n4, $FuncObj(get_static0, dummy.C.get_static0, {}), None)
          n6 <- $Call(n3, n5, None)
          TOPLEVEL[get_static0] <- n6
          n7 <- TOPLEVEL[staticmethod]
          n8 <- TOPLEVEL[abstractmethod]
          n9 <- $Call(n8, $FuncObj(get_static1, dummy.C.get_static1, {}), None)
          n10 <- $Call(n7, n9, None)
          TOPLEVEL[get_static1] <- n10
          return None


      dummy.C.get:
        b0:
          return None


      dummy.C.get_static0:
        b0:
          return None


      dummy.C.get_static1:
        b0:
          return None |}]


let%expect_test _ =
  let source =
    {|
# this PyIR.test will generate "$unknown" values since foo, x, y and z are not defined
class C:
    @foo(x, y, z)
    def f(self):
        pass

    @foo.bar(x, y, z)
    def g(self):
        pass
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), "C", None)
          TOPLEVEL[C] <- n0
          return None


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          n1 <- TOPLEVEL[foo]
          n2 <- TOPLEVEL[x]
          n3 <- TOPLEVEL[y]
          n4 <- TOPLEVEL[z]
          n5 <- $Call(n1, n2, n3, n4, None)
          n6 <- $Call(n5, $FuncObj(f, dummy.C.f, {}), None)
          TOPLEVEL[f] <- n6
          n7 <- TOPLEVEL[foo]
          n8 <- TOPLEVEL[x]
          n9 <- TOPLEVEL[y]
          n10 <- TOPLEVEL[z]
          n11 <- $CallMethod[bar](n7, n8, n9, n10, None)
          n12 <- $Call(n11, $FuncObj(g, dummy.C.g, {}), None)
          TOPLEVEL[g] <- n12
          return None


      dummy.C.f:
        b0:
          return None


      dummy.C.g:
        b0:
          return None |}]


let%expect_test _ =
  let source =
    {|
import unittest

class PwdTest(unittest.TestCase):

    def test_values(self, e):
        self.assertIn(type(e.pw_gecos), (str, type(None)))
      |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $ImportName(unittest)(None, 0, None)
          TOPLEVEL[unittest] <- n0
          n1 <- TOPLEVEL[unittest]
          n2 <- n1.TestCase
          n3 <- $BuildClass($FuncObj(PwdTest, dummy.PwdTest, {}), "PwdTest", n2, None)
          TOPLEVEL[PwdTest] <- n3
          return None


      dummy.PwdTest:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "PwdTest"
          TOPLEVEL[test_values] <- $FuncObj(test_values, dummy.PwdTest.test_values, {})
          return None


      dummy.PwdTest.test_values:
        b0:
          n0 <- LOCAL[self]
          n1 <- GLOBAL[type]
          n2 <- LOCAL[e]
          n3 <- n2.pw_gecos
          n4 <- $Call(n1, n3, None)
          n5 <- GLOBAL[str]
          n6 <- GLOBAL[type]
          n7 <- $Call(n6, None, None)
          n8 <- $CallMethod[assertIn](n0, n4, (unpacked)(n5, n7), None)
          return None |}]


let%expect_test _ =
  let source =
    {|
class C(Exception):
          pass

def f():
  raise C

def g():
  raise C()
          |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- TOPLEVEL[Exception]
          n1 <- $BuildClass($FuncObj(C, dummy.C, {}), "C", n0, None)
          TOPLEVEL[C] <- n1
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          TOPLEVEL[g] <- $FuncObj(g, dummy.g, {})
          return None


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          return None


      dummy.f:
        b0:
          n0 <- GLOBAL[C]
          throw n0


      dummy.g:
        b0:
          n0 <- GLOBAL[C]
          n1 <- $Call(n0, None)
          throw n1 |}]


let%expect_test _ =
  let source =
    {|
class C:
    pass

# TODO: we only support simple types as default arguments.
# We might add support for objects/instances if need be, in the future
def f(x, y=1, z=2, s="zuck"):
    pass

f(0)
f(10, 100)
f(100, 1000, 0)
f(0, 0, 0, "toto")
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), "C", None)
          TOPLEVEL[C] <- n0
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {(s, "zuck"); (y, 1); (z, 2); })
          n1 <- TOPLEVEL[f]
          n2 <- $Call(n1, 0, None)
          n3 <- TOPLEVEL[f]
          n4 <- $Call(n3, 10, 100, None)
          n5 <- TOPLEVEL[f]
          n6 <- $Call(n5, 100, 1000, 0, None)
          n7 <- TOPLEVEL[f]
          n8 <- $Call(n7, 0, 0, 0, "toto", None)
          return None


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          return None


      dummy.f:
        b0:
          return None |}]


let%expect_test _ =
  let source =
    {|
# From Cinder PyIR.test suite
class TestHook:
    def __init__(self, raise_on_events=None, exc_type=RuntimeError):
          pass
          |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $BuildClass($FuncObj(TestHook, dummy.TestHook, {}), "TestHook", None)
          TOPLEVEL[TestHook] <- n0
          return None


      dummy.TestHook:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "TestHook"
          n1 <- TOPLEVEL[RuntimeError]
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.TestHook.__init__, {(exc_type, n1); (raise_on_events, None); })
          return None


      dummy.TestHook.__init__:
        b0:
          return None |}]


let%expect_test _ =
  let source =
    {|
class C:
        def f(self, x, y=1, z=10):
          return x + y + z

c = C()
c.f(0)
c.f(0, 1)
c.f(0, 1, 2)
|}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), "C", None)
          TOPLEVEL[C] <- n0
          n1 <- TOPLEVEL[C]
          n2 <- $Call(n1, None)
          TOPLEVEL[c] <- n2
          n3 <- TOPLEVEL[c]
          n4 <- $CallMethod[f](n3, 0, None)
          n5 <- TOPLEVEL[c]
          n6 <- $CallMethod[f](n5, 0, 1, None)
          n7 <- TOPLEVEL[c]
          n8 <- $CallMethod[f](n7, 0, 1, 2, None)
          return None


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          TOPLEVEL[f] <- $FuncObj(f, dummy.C.f, {(y, 1); (z, 10); })
          return None


      dummy.C.f:
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[y]
          n2 <- $Binary.Add(n0, n1, None)
          n3 <- LOCAL[z]
          n4 <- $Binary.Add(n2, n3, None)
          return n4 |}]


let%expect_test _ =
  let source = {|
class C:
        x : int = 0
        |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), "C", None)
          TOPLEVEL[C] <- n0
          return None


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          $SETUP_ANNOTATIONS
          TOPLEVEL[x] <- 0
          n1 <- TOPLEVEL[int]
          n2 <- TOPLEVEL[__annotations__]
          n2["x"] <- n1
          return None |}]


let%expect_test _ =
  let source =
    {|
class defaultdict:
    def __getitem__(self, key):
        try:
            return 42
        except KeyError:
            return self.default
          |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $BuildClass($FuncObj(defaultdict, dummy.defaultdict, {}), "defaultdict", None)
          TOPLEVEL[defaultdict] <- n0
          return None


      dummy.defaultdict.__getitem__:
        b0:
          return 42


      dummy.defaultdict:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "defaultdict"
          TOPLEVEL[__getitem__] <- $FuncObj(__getitem__, dummy.defaultdict.__getitem__, {})
          return None |}]


let%expect_test _ =
  let source =
    {|
import itertools

def f():
        yield 42

class AsyncYieldFrom:
    def __await__(self):
        yield from self.obj

def powerset(s):
    for i in range(len(s)+1):
        yield from map(frozenset, itertools.combinations(s, i))

        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $ImportName(itertools)(None, 0, None)
          TOPLEVEL[itertools] <- n0
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          n1 <- $BuildClass($FuncObj(AsyncYieldFrom, dummy.AsyncYieldFrom, {}), "AsyncYieldFrom", None)
          TOPLEVEL[AsyncYieldFrom] <- n1
          TOPLEVEL[powerset] <- $FuncObj(powerset, dummy.powerset, {})
          return None


      dummy.AsyncYieldFrom:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "AsyncYieldFrom"
          TOPLEVEL[__await__] <- $FuncObj(__await__, dummy.AsyncYieldFrom.__await__, {})
          return None


      dummy.AsyncYieldFrom.__await__:
        b0:
          n0 <- LOCAL[self]
          n1 <- n0.obj
          n2 <- $GetYieldFromIter(n1, None)
          n3 <- $YieldFrom(n2, None, None)
          return None


      dummy.f:
        b0:
          n0 <- $Yield(42)
          return None


      dummy.powerset:
        b0:
          n0 <- GLOBAL[range]
          n1 <- GLOBAL[len]
          n2 <- LOCAL[s]
          n3 <- $Call(n1, n2, None)
          n4 <- $Binary.Add(n3, 1, None)
          n5 <- $Call(n0, n4, None)
          n6 <- $GetIter(n5, None)
          jmp b1

        b1:
          n7 <- $NextIter(n6, None)
          n8 <- $HasNextIter(n6, None)
          if n8 then jmp b2 else jmp b3

        b2:
          LOCAL[i] <- n7
          n9 <- GLOBAL[map]
          n10 <- GLOBAL[frozenset]
          n11 <- GLOBAL[itertools]
          n12 <- LOCAL[s]
          n13 <- LOCAL[i]
          n14 <- $CallMethod[combinations](n11, n12, n13, None)
          n15 <- $Call(n9, n10, n14, None)
          n16 <- $GetYieldFromIter(n15, None)
          n17 <- $YieldFrom(n16, None, None)
          jmp b1

        b3:
          return None |}]


let%expect_test _ =
  let source =
    {|
class C:
        pass

c = C()
del c

c0 = C()

del c0.foo

def f(x):
        global c0
        del c0
        del x

        z = 0
        def inner():
          nonlocal z
          print(z)
          del z

def g(a, b):
        del a[b]
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), "C", None)
          TOPLEVEL[C] <- n0
          n1 <- TOPLEVEL[C]
          n2 <- $Call(n1, None)
          TOPLEVEL[c] <- n2
          n3 <- $Delete(TOPLEVEL[c])(None)
          n4 <- TOPLEVEL[C]
          n5 <- $Call(n4, None)
          GLOBAL[c0] <- n5
          n6 <- GLOBAL[c0]
          n7 <- $DeleteAttr(foo)(n6, None)
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          TOPLEVEL[g] <- $FuncObj(g, dummy.g, {})
          return None


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          return None


      dummy.f:
        b0:
          n0 <- $Delete(GLOBAL[c0])(None)
          n1 <- $Delete(LOCAL[x])(None)
          n2 <- $StoreDeref[0,"z"](0, None)
          n3 <- $LoadClosure[0,"z"](None)
          LOCAL[inner] <- $FuncObj(inner, dummy.f.inner, {})
          return None


      dummy.g:
        b0:
          n0 <- LOCAL[a]
          n1 <- LOCAL[b]
          n2 <- $DeleteSubscr(n0, n1, None)
          return None


      dummy.f.inner:
        b0:
          n0 <- GLOBAL[print]
          n1 <- $LoadDeref[0,"z"](None)
          n2 <- $Call(n0, n1, None)
          n3 <- $DeleteDeref[0,"z")(None)
          return None |}]


let%expect_test _ =
  let source =
    {|
class C:
    def f(self):
        import binascii

        class D:
            def g(self, unhexlify=binascii.unhexlify):
                pass
        |}
  in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), "C", None)
          TOPLEVEL[C] <- n0
          return None


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          TOPLEVEL[f] <- $FuncObj(f, dummy.C.f, {})
          return None


      dummy.C.f.D:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C.f.<locals>.D"
          n1 <- $LoadClassDeref[0,"binascii"](None)
          n2 <- n1.unhexlify
          TOPLEVEL[g] <- $FuncObj(g, dummy.C.f.D.g, {(unhexlify, n2); })
          return None


      dummy.C.f:
        b0:
          n0 <- $ImportName(binascii)(None, 0, None)
          n1 <- $StoreDeref[0,"binascii"](n0, None)
          n2 <- $LoadClosure[0,"binascii"](None)
          n3 <- $BuildClass($FuncObj(D, dummy.C.f.D, {}), "D", None)
          LOCAL[D] <- n3
          return None


      dummy.C.f.D.g:
        b0:
          return None |}]


let%expect_test _ =
  let source = {|
class C(metaclass=m):
    pass
        |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      toplevel:
        b0:
          n0 <- TOPLEVEL[m]
          n1 <- $BuildClass($FuncObj(C, dummy.C, {}), "C", n0, ("metaclass"))
          TOPLEVEL[C] <- n1
          return None


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          return None |}]
