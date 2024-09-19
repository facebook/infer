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
          C(PyIR.Name) <- n0
          n1 <- C(PyIR.Name)(PYCInt (0), PYCString ("a"))
          c(PyIR.Name) <- n1
          n2 <- c(PyIR.Name).x
          n3 <- $CallMethod($LoadMethod(c(PyIR.Name), get), )
          n4 <- $CallMethod($LoadMethod(c(PyIR.Name), set), PYCInt (42))
          return PYCNone


      dummy.C:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("C")
          __init__(PyIR.Name) <- $FuncObj(__init__, dummy.C.__init__, {})
          get(PyIR.Name) <- $FuncObj(get, dummy.C.get, {})
          set(PyIR.Name) <- $FuncObj(set, dummy.C.set, {})
          return PYCNone


      dummy.C.__init__:
        b0:
          self(PyIR.Fast).x <- x(PyIR.Fast)
          self(PyIR.Fast).y <- y(PyIR.Fast)
          return PYCNone


      dummy.C.get:
        b0:
          return self(PyIR.Fast).x


      dummy.C.set:
        b0:
          self(PyIR.Fast).x <- x(PyIR.Fast)
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
          IntBox(PyIR.Name) <- n0
          getX(PyIR.Name) <- $FuncObj(getX, dummy.getX, {})
          n1 <- IntBox(PyIR.Name)(PYCInt (10))
          c(PyIR.Name) <- n1
          n2 <- c(PyIR.Name).x
          c(PyIR.Name).z <- PYCInt (10)
          n3 <- $CallMethod($LoadMethod(c(PyIR.Name), get), )
          n4 <- $CallMethod($LoadMethod(c(PyIR.Name), set), PYCInt (42))
          n5 <- $CallMethod($LoadMethod(c(PyIR.Name), run), )
          n6 <- print(PyIR.Name)(c(PyIR.Name).z)
          return PYCNone


      dummy.IntBox:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("IntBox")
          $SETUP_ANNOTATIONS
          __annotations__(PyIR.Name)[PYCString ("x")] <- int(PyIR.Name)
          __init__(PyIR.Name) <- $FuncObj(__init__, dummy.IntBox.__init__, {})
          get(PyIR.Name) <- $FuncObj(get, dummy.IntBox.get, {})
          set(PyIR.Name) <- $FuncObj(set, dummy.IntBox.set, {})
          run(PyIR.Name) <- $FuncObj(run, dummy.IntBox.run, {})
          n0 <- staticmethod(PyIR.Name)($FuncObj(id, dummy.IntBox.id, {}))
          id(PyIR.Name) <- n0
          return PYCNone


      dummy.IntBox.__init__:
        b0:
          self(PyIR.Fast).x <- x(PyIR.Fast)
          return PYCNone


      dummy.IntBox.get:
        b0:
          return self(PyIR.Fast).x


      dummy.getX:
        b0:
          n0 <- $CallMethod($LoadMethod(box(PyIR.Fast), get), )
          return n0


      dummy.IntBox.id:
        b0:
          return x(PyIR.Fast)


      dummy.IntBox.run:
        b0:
          return PYCNone


      dummy.IntBox.set:
        b0:
          self(PyIR.Fast).x <- x(PyIR.Fast)
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
          C(PyIR.Name) <- n0
          n1 <- $BuildClass($FuncObj(D, dummy.D, {}), PYCString ("D"), C(PyIR.Name))
          D(PyIR.Name) <- n1
          return PYCNone


      dummy.C:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("C")
          n0 <- staticmethod(PyIR.Name)($FuncObj(f, dummy.C.f, {}))
          f(PyIR.Name) <- n0
          n1 <- staticmethod(PyIR.Name)($FuncObj(typed_f, dummy.C.typed_f, {}))
          typed_f(PyIR.Name) <- n1
          return PYCNone


      dummy.D:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("D")
          return PYCNone


      dummy.C.f:
        b0:
          return PYCNone


      dummy.C.typed_f:
        b0:
          return x(PyIR.Fast) |}]


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
          C(PyIR.Name) <- n0
          n1 <- $CallMethod($LoadMethod(C(PyIR.Name), f), )
          return PYCNone


      dummy.C:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("C")
          n0 <- staticmethod(PyIR.Name)($FuncObj(f, dummy.C.f, {}))
          f(PyIR.Name) <- n0
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
          A(PyIR.Name) <- n0
          n1 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"))
          C(PyIR.Name) <- n1
          g(PyIR.Name) <- $FuncObj(g, dummy.g, {})
          return PYCNone


      dummy.A:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("A")
          f(PyIR.Name) <- $FuncObj(f, dummy.A.f, {})
          return PYCNone


      dummy.C:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("C")
          $SETUP_ANNOTATIONS
          __annotations__(PyIR.Name)[PYCString ("a")] <- A(PyIR.Name)
          return PYCNone


      dummy.A.f:
        b0:
          return PYCNone


      dummy.g:
        b0:
          n0 <- $CallMethod($LoadMethod(c(PyIR.Fast).a, f), )
          n1 <- print(PyIR.Global)(n0)
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
          A(PyIR.Name) <- n0
          n1 <- $BuildClass($FuncObj(B, dummy.B, {}), PYCString ("B"))
          B(PyIR.Name) <- n1
          n2 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"), A(PyIR.Name), B(PyIR.Name))
          C(PyIR.Name) <- n2
          return PYCNone


      dummy.A:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("A")
          return PYCNone


      dummy.B:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("B")
          return PYCNone


      dummy.C:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("C")
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
          C(PyIR.Name) <- n0
          build(PyIR.Name) <- $FuncObj(build, dummy.build, {})
          n1 <- build(PyIR.Name)()
          cs(PyIR.Name) <- n1
          n2 <- cs(PyIR.Name)[PYCInt (0)].x
          return PYCNone


      dummy.C:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("C")
          __init__(PyIR.Name) <- $FuncObj(__init__, dummy.C.__init__, {})
          return PYCNone


      dummy.C.__init__:
        b0:
          self(PyIR.Fast).x <- PYCInt (0)
          return PYCNone


      dummy.build:
        b0:
          n0 <- C(PyIR.Global)()
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
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          n0 <- f(PyIR.Name)()
          return PYCNone


      dummy.f.A:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("f.<locals>.A")
          __init__(PyIR.Name) <- $FuncObj(__init__, dummy.f.A.__init__, {})
          get(PyIR.Name) <- $FuncObj(get, dummy.f.A.get, {})
          return PYCNone


      dummy.f.A.__init__:
        b0:
          self(PyIR.Fast).x <- PYCInt (0)
          return PYCNone


      dummy.f:
        b0:
          n0 <- $BuildClass($FuncObj(A, dummy.f.A, {}), PYCString ("A"))
          A(PyIR.Fast) <- n0
          n1 <- A(PyIR.Fast)()
          a(PyIR.Fast) <- n1
          n2 <- $CallMethod($LoadMethod(a(PyIR.Fast), get), )
          return n2


      dummy.f.A.get:
        b0:
          return self(PyIR.Fast).x |}]


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
          C(PyIR.Name) <- n0
          n1 <- $BuildClass($FuncObj(D, dummy.D, {}), PYCString ("D"), C(PyIR.Name))
          D(PyIR.Name) <- n1
          return PYCNone


      dummy.C:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("C")
          return PYCNone


      dummy.D:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("D")
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
          C(PyIR.Name) <- n0
          n1 <- $BuildClass($FuncObj(D, dummy.D, {}), PYCString ("D"), C(PyIR.Name))
          D(PyIR.Name) <- n1
          n2 <- $BuildClass($FuncObj(C0, dummy.C0, {}), PYCString ("C0"))
          C0(PyIR.Name) <- n2
          n3 <- $BuildClass($FuncObj(D0, dummy.D0, {}), PYCString ("D0"), C0(PyIR.Name))
          D0(PyIR.Name) <- n3
          return PYCNone


      dummy.C:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("C")
          return PYCNone


      dummy.C0:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("C0")
          __init__(PyIR.Name) <- $FuncObj(__init__, dummy.C0.__init__, {})
          return PYCNone


      dummy.D:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("D")
          __init__(PyIR.Name) <- $FuncObj(__init__, dummy.D.__init__, {})
          __classcell__(PyIR.Name) <- $Ref(__class__)
          return $Ref(__class__)


      dummy.D0:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("D0")
          __init__(PyIR.Name) <- $FuncObj(__init__, dummy.D0.__init__, {})
          __classcell__(PyIR.Name) <- $Ref(__class__)
          return $Ref(__class__)


      dummy.C0.__init__:
        b0:
          foo(PyIR.Fast).x <- x(PyIR.Fast)
          return PYCNone


      dummy.D.__init__:
        b0:
          n0 <- super(PyIR.Global)()
          n1 <- $CallMethod($LoadMethod(n0, __init__), )
          return PYCNone


      dummy.D0.__init__:
        b0:
          n0 <- super(PyIR.Global)()
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
          foo(PyIR.Name) <- n0
          n1 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"), foo(PyIR.Name).D)
          C(PyIR.Name) <- n1
          return PYCNone


      dummy.C:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("C")
          __init__(PyIR.Name) <- $FuncObj(__init__, dummy.C.__init__, {})
          __classcell__(PyIR.Name) <- $Ref(__class__)
          return $Ref(__class__)


      dummy.C.__init__:
        b0:
          n0 <- super(PyIR.Global)()
          n1 <- $CallMethod($LoadMethod(n0, __init__), x(PyIR.Fast))
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
          ABC(PyIR.Name) <- n1
          n2 <- $ImportFrom(abstractmethod)(n0)
          abstractmethod(PyIR.Name) <- n2
          n3 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"), ABC(PyIR.Name))
          C(PyIR.Name) <- n3
          return PYCNone


      dummy.C:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("C")
          n0 <- abstractmethod(PyIR.Name)($FuncObj(get, dummy.C.get, {}))
          get(PyIR.Name) <- n0
          n1 <- staticmethod(PyIR.Name)($FuncObj(get_static0, dummy.C.get_static0, {}))
          n2 <- abstractmethod(PyIR.Name)(n1)
          get_static0(PyIR.Name) <- n2
          n3 <- abstractmethod(PyIR.Name)($FuncObj(get_static1, dummy.C.get_static1, {}))
          n4 <- staticmethod(PyIR.Name)(n3)
          get_static1(PyIR.Name) <- n4
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
          C(PyIR.Name) <- n0
          return PYCNone


      dummy.C:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("C")
          n0 <- foo(PyIR.Name)(x(PyIR.Name), y(PyIR.Name), z(PyIR.Name))
          n1 <- n0($FuncObj(f, dummy.C.f, {}))
          f(PyIR.Name) <- n1
          n2 <- $CallMethod($LoadMethod(foo(PyIR.Name), bar), x(PyIR.Name), y(PyIR.Name), z(PyIR.Name))
          n3 <- n2($FuncObj(g, dummy.C.g, {}))
          g(PyIR.Name) <- n3
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
          unittest(PyIR.Name) <- n0
          n1 <- $BuildClass($FuncObj(PwdTest, dummy.PwdTest, {}), PYCString ("PwdTest"), unittest(PyIR.Name).TestCase)
          PwdTest(PyIR.Name) <- n1
          return PYCNone


      dummy.PwdTest:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("PwdTest")
          test_values(PyIR.Name) <- $FuncObj(test_values, dummy.PwdTest.test_values, {})
          return PYCNone


      dummy.PwdTest.test_values:
        b0:
          n0 <- type(PyIR.Global)(e(PyIR.Fast).pw_gecos)
          n1 <- type(PyIR.Global)(PYCNone)
          n2 <- $CallMethod($LoadMethod(self(PyIR.Fast), assertIn), n0, (str(PyIR.Global), n1))
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
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"), Exception(PyIR.Name))
          C(PyIR.Name) <- n0
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          g(PyIR.Name) <- $FuncObj(g, dummy.g, {})
          return PYCNone


      dummy.C:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("C")
          return PYCNone


      dummy.f:
        b0:
          throw C(PyIR.Global)


      dummy.g:
        b0:
          n0 <- C(PyIR.Global)()
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
          C(PyIR.Name) <- n0
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {(s, PYCString ("zuck")); (y,
            PYCInt (1)); (z, PYCInt (2)); })
          n1 <- f(PyIR.Name)(PYCInt (0))
          n2 <- f(PyIR.Name)(PYCInt (10), PYCInt (100))
          n3 <- f(PyIR.Name)(PYCInt (100), PYCInt (1000), PYCInt (0))
          n4 <- f(PyIR.Name)(PYCInt (0), PYCInt (0), PYCInt (0), PYCString ("toto"))
          return PYCNone


      dummy.C:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("C")
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
          TestHook(PyIR.Name) <- n0
          return PYCNone


      dummy.TestHook:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("TestHook")
          __init__(PyIR.Name) <- $FuncObj(__init__, dummy.TestHook.__init__, {(exc_type, RuntimeError(PyIR.Name)); (raise_on_events, PYCNone); })
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
          C(PyIR.Name) <- n0
          n1 <- C(PyIR.Name)()
          c(PyIR.Name) <- n1
          n2 <- $CallMethod($LoadMethod(c(PyIR.Name), f), PYCInt (0))
          n3 <- $CallMethod($LoadMethod(c(PyIR.Name), f), PYCInt (0), PYCInt (1))
          n4 <- $CallMethod($LoadMethod(c(PyIR.Name), f), PYCInt (0), PYCInt (1), PYCInt (2))
          return PYCNone


      dummy.C:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("C")
          f(PyIR.Name) <- $FuncObj(f, dummy.C.f, {(y, PYCInt (1)); (z, PYCInt (10)); })
          return PYCNone


      dummy.C.f:
        b0:
          n0 <- $Binary.Add(x(PyIR.Fast), y(PyIR.Fast))
          n1 <- $Binary.Add(n0, z(PyIR.Fast))
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
          C(PyIR.Name) <- n0
          return PYCNone


      dummy.C:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("C")
          $SETUP_ANNOTATIONS
          x(PyIR.Name) <- PYCInt (0)
          __annotations__(PyIR.Name)[PYCString ("x")] <- int(PyIR.Name)
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
          defaultdict(PyIR.Name) <- n0
          return PYCNone


      dummy.defaultdict.__getitem__:
        b0:
          return PYCInt (42)

        b1:
          n6 <- $Compare.exception(n5, KeyError(PyIR.Global))
          if n6 then jmp b2(n5, n4, n3, n2, n1, n0) else jmp b3(n5, n4, n3, n2, n1, n0)

        b2:
          jmp b4(self(PyIR.Fast).default)

        b3:
          return PYCNone

        b4:
          return n19


      dummy.defaultdict:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("defaultdict")
          __getitem__(PyIR.Name) <- $FuncObj(__getitem__, dummy.defaultdict.__getitem__, {})
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
          itertools(PyIR.Name) <- n0
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          n1 <- $BuildClass($FuncObj(AsyncYieldFrom, dummy.AsyncYieldFrom, {}),
                            PYCString ("AsyncYieldFrom"))
          AsyncYieldFrom(PyIR.Name) <- n1
          powerset(PyIR.Name) <- $FuncObj(powerset, dummy.powerset, {})
          return PYCNone


      dummy.AsyncYieldFrom:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("AsyncYieldFrom")
          __await__(PyIR.Name) <- $FuncObj(__await__, dummy.AsyncYieldFrom.__await__, {})
          return PYCNone


      dummy.AsyncYieldFrom.__await__:
        b0:
          n0 <- $GetYieldFromIter(self(PyIR.Fast).obj)
          n1 <- $YieldFrom(n0, PYCNone)
          return PYCNone


      dummy.f:
        b0:
          n0 <- $Yield(PYCInt (42))
          return PYCNone


      dummy.powerset:
        b0:
          n0 <- len(PyIR.Global)(s(PyIR.Fast))
          n1 <- $Binary.Add(n0, PYCInt (1))
          n2 <- range(PyIR.Global)(n1)
          n3 <- $GetIter(n2)
          jmp b1(n3)

        b1:
          n5 <- $NextIter(n4)
          n6 <- $HasNextIter(n5)
          if n6 then jmp b2 else jmp b3

        b2:
          n7 <- $IterData(n5)
          i(PyIR.Fast) <- n7
          n8 <- $CallMethod($LoadMethod(itertools(PyIR.Global), combinations),
            s(PyIR.Fast), i(PyIR.Fast))
          n9 <- map(PyIR.Global)(frozenset(PyIR.Global), n8)
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
          C(PyIR.Name) <- n0
          n1 <- C(PyIR.Name)()
          c(PyIR.Name) <- n1
          n2 <- $DeletePyIR.Name(c)()
          n3 <- C(PyIR.Name)()
          c0(PyIR.Global) <- n3
          n4 <- $DeleteAttr(foo)(c0(PyIR.Global))
          f(PyIR.Name) <- $FuncObj(f, dummy.f, {})
          g(PyIR.Name) <- $FuncObj(g, dummy.g, {})
          return PYCNone


      dummy.C:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("C")
          return PYCNone


      dummy.f:
        b0:
          n0 <- $DeletePyIR.Global(c0)()
          n1 <- $DeletePyIR.Fast(x)()
          z(PyIR.Deref) <- PYCInt (0)
          inner(PyIR.Fast) <- $FuncObj(inner, dummy.f.inner, {})
          return PYCNone


      dummy.g:
        b0:
          n0 <- $DeleteSubscr(a(PyIR.Fast), b(PyIR.Fast))
          return PYCNone


      dummy.f.inner:
        b0:
          n0 <- $DeletePyIR.Deref(z)()
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
          C(PyIR.Name) <- n0
          return PYCNone


      dummy.C:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("C")
          f(PyIR.Name) <- $FuncObj(f, dummy.C.f, {})
          return PYCNone


      dummy.C.f.D:
        b0:
          __module__(PyIR.Name) <- __name__(PyIR.Name)
          __qualname__(PyIR.Name) <- PYCString ("C.f.<locals>.D")
          g(PyIR.Name) <- $FuncObj(g, dummy.C.f.D.g, {(unhexlify, binascii(PyIR.Deref).unhexlify); })
          return PYCNone


      dummy.C.f:
        b0:
          n0 <- $ImportName(binascii)(PYCNone, PYCInt (0))
          binascii(PyIR.Deref) <- n0
          n1 <- $BuildClass($FuncObj(D, dummy.C.f.D, {}), PYCString ("D"))
          D(PyIR.Fast) <- n1
          return PYCNone


      dummy.C.f.D.g:
        b0:
          return PYCNone |}]
