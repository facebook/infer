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
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"))
          TOPLEVEL[C] <- n0
          n1 <- TOPLEVEL[C](PYCInt (0), PYCString ("a"))
          TOPLEVEL[c] <- n1
          n2 <- TOPLEVEL[c].x
          n3 <- $CallMethod($LoadMethod(TOPLEVEL[c], get), )
          n4 <- $CallMethod($LoadMethod(TOPLEVEL[c], set), PYCInt (42))
          return PYCNone


      dummy.C:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("C")
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.C.__init__, {})
          TOPLEVEL[get] <- $FuncObj(get, dummy.C.get, {})
          TOPLEVEL[set] <- $FuncObj(set, dummy.C.set, {})
          return PYCNone


      dummy.C.__init__:
        b0:
          LOCAL[self].x <- LOCAL[x]
          LOCAL[self].y <- LOCAL[y]
          return PYCNone


      dummy.C.get:
        b0:
          return LOCAL[self].x


      dummy.C.set:
        b0:
          LOCAL[self].x <- LOCAL[x]
          return PYCNone |}]


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
          n0 <- $BuildClass($FuncObj(IntBox, dummy.IntBox, {}), PYCString ("IntBox"))
          TOPLEVEL[IntBox] <- n0
          TOPLEVEL[getX] <- $FuncObj(getX, dummy.getX, {})
          n1 <- TOPLEVEL[IntBox](PYCInt (10))
          TOPLEVEL[c] <- n1
          n2 <- TOPLEVEL[c].x
          TOPLEVEL[c].z <- PYCInt (10)
          n3 <- $CallMethod($LoadMethod(TOPLEVEL[c], get), )
          n4 <- $CallMethod($LoadMethod(TOPLEVEL[c], set), PYCInt (42))
          n5 <- $CallMethod($LoadMethod(TOPLEVEL[c], run), )
          n6 <- TOPLEVEL[print](TOPLEVEL[c].z)
          return PYCNone


      dummy.IntBox:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("IntBox")
          $SETUP_ANNOTATIONS
          TOPLEVEL[__annotations__][PYCString ("x")] <- TOPLEVEL[int]
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.IntBox.__init__, {})
          TOPLEVEL[get] <- $FuncObj(get, dummy.IntBox.get, {})
          TOPLEVEL[set] <- $FuncObj(set, dummy.IntBox.set, {})
          TOPLEVEL[run] <- $FuncObj(run, dummy.IntBox.run, {})
          n0 <- TOPLEVEL[staticmethod]($FuncObj(id, dummy.IntBox.id, {}))
          TOPLEVEL[id] <- n0
          return PYCNone


      dummy.IntBox.__init__:
        b0:
          LOCAL[self].x <- LOCAL[x]
          return PYCNone


      dummy.IntBox.get:
        b0:
          return LOCAL[self].x


      dummy.getX:
        b0:
          n0 <- $CallMethod($LoadMethod(LOCAL[box], get), )
          return n0


      dummy.IntBox.id:
        b0:
          return LOCAL[x]


      dummy.IntBox.run:
        b0:
          return PYCNone


      dummy.IntBox.set:
        b0:
          LOCAL[self].x <- LOCAL[x]
          return PYCNone |}]


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
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"))
          TOPLEVEL[C] <- n0
          n1 <- $BuildClass($FuncObj(D, dummy.D, {}), PYCString ("D"), TOPLEVEL[C])
          TOPLEVEL[D] <- n1
          return PYCNone


      dummy.C:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("C")
          n0 <- TOPLEVEL[staticmethod]($FuncObj(f, dummy.C.f, {}))
          TOPLEVEL[f] <- n0
          n1 <- TOPLEVEL[staticmethod]($FuncObj(typed_f, dummy.C.typed_f, {}))
          TOPLEVEL[typed_f] <- n1
          return PYCNone


      dummy.D:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("D")
          return PYCNone


      dummy.C.f:
        b0:
          return PYCNone


      dummy.C.typed_f:
        b0:
          return LOCAL[x] |}]


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
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"))
          TOPLEVEL[C] <- n0
          n1 <- $CallMethod($LoadMethod(TOPLEVEL[C], f), )
          return PYCNone


      dummy.C:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("C")
          n0 <- TOPLEVEL[staticmethod]($FuncObj(f, dummy.C.f, {}))
          TOPLEVEL[f] <- n0
          return PYCNone


      dummy.C.f:
        b0:
          return PYCNone |}]


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
          n0 <- $BuildClass($FuncObj(A, dummy.A, {}), PYCString ("A"))
          TOPLEVEL[A] <- n0
          n1 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"))
          TOPLEVEL[C] <- n1
          TOPLEVEL[g] <- $FuncObj(g, dummy.g, {})
          return PYCNone


      dummy.A:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("A")
          TOPLEVEL[f] <- $FuncObj(f, dummy.A.f, {})
          return PYCNone


      dummy.C:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("C")
          $SETUP_ANNOTATIONS
          TOPLEVEL[__annotations__][PYCString ("a")] <- TOPLEVEL[A]
          return PYCNone


      dummy.A.f:
        b0:
          return PYCNone


      dummy.g:
        b0:
          n0 <- $CallMethod($LoadMethod(LOCAL[c].a, f), )
          n1 <- GLOBAL[print](n0)
          return PYCNone |}]


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
          n0 <- $BuildClass($FuncObj(A, dummy.A, {}), PYCString ("A"))
          TOPLEVEL[A] <- n0
          n1 <- $BuildClass($FuncObj(B, dummy.B, {}), PYCString ("B"))
          TOPLEVEL[B] <- n1
          n2 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"), TOPLEVEL[A], TOPLEVEL[B])
          TOPLEVEL[C] <- n2
          return PYCNone


      dummy.A:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("A")
          return PYCNone


      dummy.B:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("B")
          return PYCNone


      dummy.C:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("C")
          return PYCNone |}]


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
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"))
          TOPLEVEL[C] <- n0
          TOPLEVEL[build] <- $FuncObj(build, dummy.build, {})
          n1 <- TOPLEVEL[build]()
          TOPLEVEL[cs] <- n1
          n2 <- TOPLEVEL[cs][PYCInt (0)].x
          return PYCNone


      dummy.C:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("C")
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.C.__init__, {})
          return PYCNone


      dummy.C.__init__:
        b0:
          LOCAL[self].x <- PYCInt (0)
          return PYCNone


      dummy.build:
        b0:
          n0 <- GLOBAL[C]()
          return [n0] |}]


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
          n0 <- TOPLEVEL[f]()
          return PYCNone


      dummy.f.A:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("f.<locals>.A")
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.f.A.__init__, {})
          TOPLEVEL[get] <- $FuncObj(get, dummy.f.A.get, {})
          return PYCNone


      dummy.f.A.__init__:
        b0:
          LOCAL[self].x <- PYCInt (0)
          return PYCNone


      dummy.f:
        b0:
          n0 <- $BuildClass($FuncObj(A, dummy.f.A, {}), PYCString ("A"))
          LOCAL[A] <- n0
          n1 <- LOCAL[A]()
          LOCAL[a] <- n1
          n2 <- $CallMethod($LoadMethod(LOCAL[a], get), )
          return n2


      dummy.f.A.get:
        b0:
          return LOCAL[self].x |}]


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
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"))
          TOPLEVEL[C] <- n0
          n1 <- $BuildClass($FuncObj(D, dummy.D, {}), PYCString ("D"), TOPLEVEL[C])
          TOPLEVEL[D] <- n1
          return PYCNone


      dummy.C:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("C")
          return PYCNone


      dummy.D:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("D")
          return PYCNone |}]


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
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"))
          TOPLEVEL[C] <- n0
          n1 <- $BuildClass($FuncObj(D, dummy.D, {}), PYCString ("D"), TOPLEVEL[C])
          TOPLEVEL[D] <- n1
          n2 <- $BuildClass($FuncObj(C0, dummy.C0, {}), PYCString ("C0"))
          TOPLEVEL[C0] <- n2
          n3 <- $BuildClass($FuncObj(D0, dummy.D0, {}), PYCString ("D0"), TOPLEVEL[C0])
          TOPLEVEL[D0] <- n3
          return PYCNone


      dummy.C:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("C")
          return PYCNone


      dummy.C0:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("C0")
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.C0.__init__, {})
          return PYCNone


      dummy.D:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("D")
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.D.__init__, {})
          TOPLEVEL[__classcell__] <- $Ref(__class__)
          return $Ref(__class__)


      dummy.D0:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("D0")
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.D0.__init__, {})
          TOPLEVEL[__classcell__] <- $Ref(__class__)
          return $Ref(__class__)


      dummy.C0.__init__:
        b0:
          LOCAL[foo].x <- LOCAL[x]
          return PYCNone


      dummy.D.__init__:
        b0:
          n0 <- GLOBAL[super]()
          n1 <- $CallMethod($LoadMethod(n0, __init__), )
          return PYCNone


      dummy.D0.__init__:
        b0:
          n0 <- GLOBAL[super]()
          n1 <- $CallMethod($LoadMethod(n0, __init__), PYCInt (42))
          return PYCNone |}]


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
          n0 <- $ImportName(foo)(PYCNone, PYCInt (0))
          TOPLEVEL[foo] <- n0
          n1 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"), TOPLEVEL[foo].D)
          TOPLEVEL[C] <- n1
          return PYCNone


      dummy.C:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("C")
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.C.__init__, {})
          TOPLEVEL[__classcell__] <- $Ref(__class__)
          return $Ref(__class__)


      dummy.C.__init__:
        b0:
          n0 <- GLOBAL[super]()
          n1 <- $CallMethod($LoadMethod(n0, __init__), LOCAL[x])
          return PYCNone |}]


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
          n0 <- $ImportName(abc)(PYCTuple ([|PYCString ("ABC"); PYCString ("abstractmethod")|]),
                                 PYCInt (0))
          n1 <- $ImportFrom(ABC)(n0)
          TOPLEVEL[ABC] <- n1
          n2 <- $ImportFrom(abstractmethod)(n0)
          TOPLEVEL[abstractmethod] <- n2
          n3 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"), TOPLEVEL[ABC])
          TOPLEVEL[C] <- n3
          return PYCNone


      dummy.C:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("C")
          n0 <- TOPLEVEL[abstractmethod]($FuncObj(get, dummy.C.get, {}))
          TOPLEVEL[get] <- n0
          n1 <- TOPLEVEL[staticmethod]($FuncObj(get_static0, dummy.C.get_static0, {}))
          n2 <- TOPLEVEL[abstractmethod](n1)
          TOPLEVEL[get_static0] <- n2
          n3 <- TOPLEVEL[abstractmethod]($FuncObj(get_static1, dummy.C.get_static1, {}))
          n4 <- TOPLEVEL[staticmethod](n3)
          TOPLEVEL[get_static1] <- n4
          return PYCNone


      dummy.C.get:
        b0:
          return PYCNone


      dummy.C.get_static0:
        b0:
          return PYCNone


      dummy.C.get_static1:
        b0:
          return PYCNone |}]


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
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"))
          TOPLEVEL[C] <- n0
          return PYCNone


      dummy.C:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("C")
          n0 <- TOPLEVEL[foo](TOPLEVEL[x], TOPLEVEL[y], TOPLEVEL[z])
          n1 <- n0($FuncObj(f, dummy.C.f, {}))
          TOPLEVEL[f] <- n1
          n2 <- $CallMethod($LoadMethod(TOPLEVEL[foo], bar), TOPLEVEL[x], TOPLEVEL[y], TOPLEVEL[z])
          n3 <- n2($FuncObj(g, dummy.C.g, {}))
          TOPLEVEL[g] <- n3
          return PYCNone


      dummy.C.f:
        b0:
          return PYCNone


      dummy.C.g:
        b0:
          return PYCNone |}]


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
          n0 <- $ImportName(unittest)(PYCNone, PYCInt (0))
          TOPLEVEL[unittest] <- n0
          n1 <- $BuildClass($FuncObj(PwdTest, dummy.PwdTest, {}), PYCString ("PwdTest"), TOPLEVEL[unittest].TestCase)
          TOPLEVEL[PwdTest] <- n1
          return PYCNone


      dummy.PwdTest:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("PwdTest")
          TOPLEVEL[test_values] <- $FuncObj(test_values, dummy.PwdTest.test_values, {})
          return PYCNone


      dummy.PwdTest.test_values:
        b0:
          n0 <- GLOBAL[type](LOCAL[e].pw_gecos)
          n1 <- GLOBAL[type](PYCNone)
          n2 <- $CallMethod($LoadMethod(LOCAL[self], assertIn), n0, (GLOBAL[str], n1))
          return PYCNone |}]


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
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"), TOPLEVEL[Exception])
          TOPLEVEL[C] <- n0
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          TOPLEVEL[g] <- $FuncObj(g, dummy.g, {})
          return PYCNone


      dummy.C:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("C")
          return PYCNone


      dummy.f:
        b0:
          throw GLOBAL[C]


      dummy.g:
        b0:
          n0 <- GLOBAL[C]()
          throw n0 |}]


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
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"))
          TOPLEVEL[C] <- n0
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {(s, PYCString ("zuck")); (y,
            PYCInt (1)); (z, PYCInt (2)); })
          n1 <- TOPLEVEL[f](PYCInt (0))
          n2 <- TOPLEVEL[f](PYCInt (10), PYCInt (100))
          n3 <- TOPLEVEL[f](PYCInt (100), PYCInt (1000), PYCInt (0))
          n4 <- TOPLEVEL[f](PYCInt (0), PYCInt (0), PYCInt (0), PYCString ("toto"))
          return PYCNone


      dummy.C:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("C")
          return PYCNone


      dummy.f:
        b0:
          return PYCNone |}]


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
          n0 <- $BuildClass($FuncObj(TestHook, dummy.TestHook, {}), PYCString ("TestHook"))
          TOPLEVEL[TestHook] <- n0
          return PYCNone


      dummy.TestHook:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("TestHook")
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.TestHook.__init__, {(exc_type, TOPLEVEL[RuntimeError]); (raise_on_events, PYCNone); })
          return PYCNone


      dummy.TestHook.__init__:
        b0:
          return PYCNone |}]


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
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"))
          TOPLEVEL[C] <- n0
          n1 <- TOPLEVEL[C]()
          TOPLEVEL[c] <- n1
          n2 <- $CallMethod($LoadMethod(TOPLEVEL[c], f), PYCInt (0))
          n3 <- $CallMethod($LoadMethod(TOPLEVEL[c], f), PYCInt (0), PYCInt (1))
          n4 <- $CallMethod($LoadMethod(TOPLEVEL[c], f), PYCInt (0), PYCInt (1), PYCInt (2))
          return PYCNone


      dummy.C:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("C")
          TOPLEVEL[f] <- $FuncObj(f, dummy.C.f, {(y, PYCInt (1)); (z, PYCInt (10)); })
          return PYCNone


      dummy.C.f:
        b0:
          n0 <- $Binary.Add(LOCAL[x], LOCAL[y])
          n1 <- $Binary.Add(n0, LOCAL[z])
          return n1 |}]


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
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"))
          TOPLEVEL[C] <- n0
          return PYCNone


      dummy.C:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("C")
          $SETUP_ANNOTATIONS
          TOPLEVEL[x] <- PYCInt (0)
          TOPLEVEL[__annotations__][PYCString ("x")] <- TOPLEVEL[int]
          return PYCNone |}]


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
          n0 <- $BuildClass($FuncObj(defaultdict, dummy.defaultdict, {}), PYCString ("defaultdict"))
          TOPLEVEL[defaultdict] <- n0
          return PYCNone


      dummy.defaultdict.__getitem__:
        b0:
          return PYCInt (42)

        b1:
          n6 <- $Compare.exception(n5, GLOBAL[KeyError])
          if n6 then jmp b2(n5, n4, n3, n2, n1, n0) else jmp b3(n5, n4, n3, n2, n1, n0)

        b2:
          jmp b4(LOCAL[self].default)

        b3:
          return PYCNone

        b4:
          return n19


      dummy.defaultdict:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("defaultdict")
          TOPLEVEL[__getitem__] <- $FuncObj(__getitem__, dummy.defaultdict.__getitem__, {})
          return PYCNone |}]


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
          n0 <- $ImportName(itertools)(PYCNone, PYCInt (0))
          TOPLEVEL[itertools] <- n0
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          n1 <- $BuildClass($FuncObj(AsyncYieldFrom, dummy.AsyncYieldFrom, {}),
                            PYCString ("AsyncYieldFrom"))
          TOPLEVEL[AsyncYieldFrom] <- n1
          TOPLEVEL[powerset] <- $FuncObj(powerset, dummy.powerset, {})
          return PYCNone


      dummy.AsyncYieldFrom:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("AsyncYieldFrom")
          TOPLEVEL[__await__] <- $FuncObj(__await__, dummy.AsyncYieldFrom.__await__, {})
          return PYCNone


      dummy.AsyncYieldFrom.__await__:
        b0:
          n0 <- $GetYieldFromIter(LOCAL[self].obj)
          n1 <- $YieldFrom(n0, PYCNone)
          return PYCNone


      dummy.f:
        b0:
          n0 <- $Yield(PYCInt (42))
          return PYCNone


      dummy.powerset:
        b0:
          n0 <- GLOBAL[len](LOCAL[s])
          n1 <- $Binary.Add(n0, PYCInt (1))
          n2 <- GLOBAL[range](n1)
          n3 <- $GetIter(n2)
          jmp b1(n3)

        b1:
          n5 <- $NextIter(n4)
          n6 <- $HasNextIter(n5)
          if n6 then jmp b2 else jmp b3

        b2:
          n7 <- $IterData(n5)
          LOCAL[i] <- n7
          n8 <- $CallMethod($LoadMethod(GLOBAL[itertools], combinations), LOCAL[s], LOCAL[i])
          n9 <- GLOBAL[map](GLOBAL[frozenset], n8)
          n10 <- $GetYieldFromIter(n9)
          n11 <- $YieldFrom(n10, PYCNone)
          jmp b1(n4)

        b3:
          return PYCNone |}]


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
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"))
          TOPLEVEL[C] <- n0
          n1 <- TOPLEVEL[C]()
          TOPLEVEL[c] <- n1
          n2 <- $Delete(TOPLEVEL[c])()
          n3 <- TOPLEVEL[C]()
          GLOBAL[c0] <- n3
          n4 <- $DeleteAttr(foo)(GLOBAL[c0])
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          TOPLEVEL[g] <- $FuncObj(g, dummy.g, {})
          return PYCNone


      dummy.C:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("C")
          return PYCNone


      dummy.f:
        b0:
          n0 <- $Delete(GLOBAL[c0])()
          n1 <- $Delete(LOCAL[x])()
          DEREF[z] <- PYCInt (0)
          LOCAL[inner] <- $FuncObj(inner, dummy.f.inner, {})
          return PYCNone


      dummy.g:
        b0:
          n0 <- $DeleteSubscr(LOCAL[a], LOCAL[b])
          return PYCNone


      dummy.f.inner:
        b0:
          n0 <- $Delete(DEREF[z])()
          return PYCNone |}]


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
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"))
          TOPLEVEL[C] <- n0
          return PYCNone


      dummy.C:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("C")
          TOPLEVEL[f] <- $FuncObj(f, dummy.C.f, {})
          return PYCNone


      dummy.C.f.D:
        b0:
          TOPLEVEL[__module__] <- TOPLEVEL[__name__]
          TOPLEVEL[__qualname__] <- PYCString ("C.f.<locals>.D")
          TOPLEVEL[g] <- $FuncObj(g, dummy.C.f.D.g, {(unhexlify, DEREF[binascii].unhexlify); })
          return PYCNone


      dummy.C.f:
        b0:
          n0 <- $ImportName(binascii)(PYCNone, PYCInt (0))
          DEREF[binascii] <- n0
          n1 <- $BuildClass($FuncObj(D, dummy.C.f.D, {}), PYCString ("D"))
          LOCAL[D] <- n1
          return PYCNone


      dummy.C.f.D.g:
        b0:
          return PYCNone |}]
