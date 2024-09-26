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
          n1 <- TOPLEVEL[C]
          n2 <- n1(PYCInt (0), PYCString ("a"))
          TOPLEVEL[c] <- n2
          n3 <- TOPLEVEL[c]
          n4 <- n3.x
          n5 <- TOPLEVEL[c]
          n6 <- $CallMethod($LoadMethod(n5, get), )
          n7 <- TOPLEVEL[c]
          n8 <- $CallMethod($LoadMethod(n7, set), PYCInt (42))
          return PYCNone


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("C")
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.C.__init__, {})
          TOPLEVEL[get] <- $FuncObj(get, dummy.C.get, {})
          TOPLEVEL[set] <- $FuncObj(set, dummy.C.set, {})
          return PYCNone


      dummy.C.__init__:
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[self]
          n1.x <- n0
          n2 <- LOCAL[y]
          n3 <- LOCAL[self]
          n3.y <- n2
          return PYCNone


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
          n1 <- TOPLEVEL[IntBox]
          n2 <- TOPLEVEL[int]
          TOPLEVEL[getX] <- $FuncObj(getX, dummy.getX, {})
          n3 <- TOPLEVEL[IntBox]
          n4 <- n3(PYCInt (10))
          TOPLEVEL[c] <- n4
          n5 <- TOPLEVEL[c]
          n6 <- n5.x
          n7 <- TOPLEVEL[c]
          n7.z <- PYCInt (10)
          n8 <- TOPLEVEL[c]
          n9 <- $CallMethod($LoadMethod(n8, get), )
          n10 <- TOPLEVEL[c]
          n11 <- $CallMethod($LoadMethod(n10, set), PYCInt (42))
          n12 <- TOPLEVEL[c]
          n13 <- $CallMethod($LoadMethod(n12, run), )
          n14 <- TOPLEVEL[print]
          n15 <- TOPLEVEL[c]
          n16 <- n15.z
          n17 <- n14(n16)
          return PYCNone


      dummy.IntBox:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("IntBox")
          $SETUP_ANNOTATIONS
          n1 <- TOPLEVEL[int]
          n2 <- TOPLEVEL[__annotations__]
          n2[PYCString ("x")] <- n1
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
          n9 <- n6($FuncObj(id, dummy.IntBox.id, {}))
          TOPLEVEL[id] <- n9
          return PYCNone


      dummy.IntBox.__init__:
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[self]
          n1.x <- n0
          return PYCNone


      dummy.IntBox.get:
        b0:
          n0 <- LOCAL[self]
          n1 <- n0.x
          return n1


      dummy.getX:
        b0:
          n0 <- LOCAL[box]
          n1 <- $CallMethod($LoadMethod(n0, get), )
          return n1


      dummy.IntBox.id:
        b0:
          n0 <- LOCAL[x]
          return n0


      dummy.IntBox.run:
        b0:
          return PYCNone


      dummy.IntBox.set:
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[self]
          n1.x <- n0
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
          n1 <- TOPLEVEL[C]
          n2 <- $BuildClass($FuncObj(D, dummy.D, {}), PYCString ("D"), n1)
          TOPLEVEL[D] <- n2
          return PYCNone


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("C")
          n1 <- TOPLEVEL[staticmethod]
          n2 <- n1($FuncObj(f, dummy.C.f, {}))
          TOPLEVEL[f] <- n2
          n3 <- TOPLEVEL[staticmethod]
          n4 <- TOPLEVEL[int]
          n5 <- TOPLEVEL[int]
          n6 <- n3($FuncObj(typed_f, dummy.C.typed_f, {}))
          TOPLEVEL[typed_f] <- n6
          return PYCNone


      dummy.D:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("D")
          return PYCNone


      dummy.C.f:
        b0:
          return PYCNone


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
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"))
          TOPLEVEL[C] <- n0
          n1 <- TOPLEVEL[C]
          n2 <- $CallMethod($LoadMethod(n1, f), )
          return PYCNone


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("C")
          n1 <- TOPLEVEL[staticmethod]
          n2 <- n1($FuncObj(f, dummy.C.f, {}))
          TOPLEVEL[f] <- n2
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
          n2 <- TOPLEVEL[C]
          TOPLEVEL[g] <- $FuncObj(g, dummy.g, {})
          return PYCNone


      dummy.A:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("A")
          TOPLEVEL[f] <- $FuncObj(f, dummy.A.f, {})
          return PYCNone


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("C")
          $SETUP_ANNOTATIONS
          n1 <- TOPLEVEL[A]
          n2 <- TOPLEVEL[__annotations__]
          n2[PYCString ("a")] <- n1
          return PYCNone


      dummy.A.f:
        b0:
          return PYCNone


      dummy.g:
        b0:
          n0 <- GLOBAL[print]
          n1 <- LOCAL[c]
          n2 <- n1.a
          n3 <- $CallMethod($LoadMethod(n2, f), )
          n4 <- n0(n3)
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
          n2 <- TOPLEVEL[A]
          n3 <- TOPLEVEL[B]
          n4 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"), n2, n3)
          TOPLEVEL[C] <- n4
          return PYCNone


      dummy.A:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("A")
          return PYCNone


      dummy.B:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("B")
          return PYCNone


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
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
          n1 <- TOPLEVEL[build]
          n2 <- n1()
          TOPLEVEL[cs] <- n2
          n3 <- TOPLEVEL[cs]
          n4 <- n3[PYCInt (0)]
          n5 <- n4.x
          return PYCNone


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("C")
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.C.__init__, {})
          return PYCNone


      dummy.C.__init__:
        b0:
          n0 <- LOCAL[self]
          n0.x <- PYCInt (0)
          return PYCNone


      dummy.build:
        b0:
          n0 <- GLOBAL[C]
          n1 <- n0()
          return [n1] |}]


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
          n1 <- n0()
          return PYCNone


      dummy.f.A:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("f.<locals>.A")
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.f.A.__init__, {})
          TOPLEVEL[get] <- $FuncObj(get, dummy.f.A.get, {})
          return PYCNone


      dummy.f.A.__init__:
        b0:
          n0 <- LOCAL[self]
          n0.x <- PYCInt (0)
          return PYCNone


      dummy.f:
        b0:
          n0 <- $BuildClass($FuncObj(A, dummy.f.A, {}), PYCString ("A"))
          LOCAL[A] <- n0
          n1 <- LOCAL[A]
          n2 <- n1()
          LOCAL[a] <- n2
          n3 <- LOCAL[a]
          n4 <- $CallMethod($LoadMethod(n3, get), )
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
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"))
          TOPLEVEL[C] <- n0
          n1 <- TOPLEVEL[C]
          n2 <- $BuildClass($FuncObj(D, dummy.D, {}), PYCString ("D"), n1)
          TOPLEVEL[D] <- n2
          return PYCNone


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("C")
          return PYCNone


      dummy.D:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
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
          n1 <- TOPLEVEL[C]
          n2 <- $BuildClass($FuncObj(D, dummy.D, {}), PYCString ("D"), n1)
          TOPLEVEL[D] <- n2
          n3 <- $BuildClass($FuncObj(C0, dummy.C0, {}), PYCString ("C0"))
          TOPLEVEL[C0] <- n3
          n4 <- TOPLEVEL[C0]
          n5 <- $BuildClass($FuncObj(D0, dummy.D0, {}), PYCString ("D0"), n4)
          TOPLEVEL[D0] <- n5
          return PYCNone


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("C")
          return PYCNone


      dummy.C0:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("C0")
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.C0.__init__, {})
          return PYCNone


      dummy.D:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("D")
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.D.__init__, {})
          TOPLEVEL[__classcell__] <- $Ref(__class__)
          return $Ref(__class__)


      dummy.D0:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("D0")
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.D0.__init__, {})
          TOPLEVEL[__classcell__] <- $Ref(__class__)
          return $Ref(__class__)


      dummy.C0.__init__:
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[foo]
          n1.x <- n0
          return PYCNone


      dummy.D.__init__:
        b0:
          n0 <- GLOBAL[super]
          n1 <- n0()
          n2 <- $CallMethod($LoadMethod(n1, __init__), )
          return PYCNone


      dummy.D0.__init__:
        b0:
          n0 <- GLOBAL[super]
          n1 <- n0()
          n2 <- $CallMethod($LoadMethod(n1, __init__), PYCInt (42))
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
          n1 <- TOPLEVEL[foo]
          n2 <- n1.D
          n3 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"), n2)
          TOPLEVEL[C] <- n3
          return PYCNone


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("C")
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.C.__init__, {})
          TOPLEVEL[__classcell__] <- $Ref(__class__)
          return $Ref(__class__)


      dummy.C.__init__:
        b0:
          n0 <- GLOBAL[super]
          n1 <- n0()
          n2 <- LOCAL[x]
          n3 <- $CallMethod($LoadMethod(n1, __init__), n2)
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
          n3 <- TOPLEVEL[ABC]
          n4 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"), n3)
          TOPLEVEL[C] <- n4
          return PYCNone


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("C")
          n1 <- TOPLEVEL[abstractmethod]
          n2 <- n1($FuncObj(get, dummy.C.get, {}))
          TOPLEVEL[get] <- n2
          n3 <- TOPLEVEL[abstractmethod]
          n4 <- TOPLEVEL[staticmethod]
          n5 <- n4($FuncObj(get_static0, dummy.C.get_static0, {}))
          n6 <- n3(n5)
          TOPLEVEL[get_static0] <- n6
          n7 <- TOPLEVEL[staticmethod]
          n8 <- TOPLEVEL[abstractmethod]
          n9 <- n8($FuncObj(get_static1, dummy.C.get_static1, {}))
          n10 <- n7(n9)
          TOPLEVEL[get_static1] <- n10
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
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("C")
          n1 <- TOPLEVEL[foo]
          n2 <- TOPLEVEL[x]
          n3 <- TOPLEVEL[y]
          n4 <- TOPLEVEL[z]
          n5 <- n1(n2, n3, n4)
          n6 <- n5($FuncObj(f, dummy.C.f, {}))
          TOPLEVEL[f] <- n6
          n7 <- TOPLEVEL[foo]
          n8 <- TOPLEVEL[x]
          n9 <- TOPLEVEL[y]
          n10 <- TOPLEVEL[z]
          n11 <- $CallMethod($LoadMethod(n7, bar), n8, n9, n10)
          n12 <- n11($FuncObj(g, dummy.C.g, {}))
          TOPLEVEL[g] <- n12
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
          n1 <- TOPLEVEL[unittest]
          n2 <- n1.TestCase
          n3 <- $BuildClass($FuncObj(PwdTest, dummy.PwdTest, {}), PYCString ("PwdTest"), n2)
          TOPLEVEL[PwdTest] <- n3
          return PYCNone


      dummy.PwdTest:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("PwdTest")
          TOPLEVEL[test_values] <- $FuncObj(test_values, dummy.PwdTest.test_values, {})
          return PYCNone


      dummy.PwdTest.test_values:
        b0:
          n0 <- LOCAL[self]
          n1 <- GLOBAL[type]
          n2 <- LOCAL[e]
          n3 <- n2.pw_gecos
          n4 <- n1(n3)
          n5 <- GLOBAL[str]
          n6 <- GLOBAL[type]
          n7 <- n6(PYCNone)
          n8 <- $CallMethod($LoadMethod(n0, assertIn), n4, (n5, n7))
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
          n0 <- TOPLEVEL[Exception]
          n1 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"), n0)
          TOPLEVEL[C] <- n1
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          TOPLEVEL[g] <- $FuncObj(g, dummy.g, {})
          return PYCNone


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("C")
          return PYCNone


      dummy.f:
        b0:
          n0 <- GLOBAL[C]
          throw n0


      dummy.g:
        b0:
          n0 <- GLOBAL[C]
          n1 <- n0()
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
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"))
          TOPLEVEL[C] <- n0
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {(s, PYCString ("zuck")); (y,
            PYCInt (1)); (z, PYCInt (2)); })
          n1 <- TOPLEVEL[f]
          n2 <- n1(PYCInt (0))
          n3 <- TOPLEVEL[f]
          n4 <- n3(PYCInt (10), PYCInt (100))
          n5 <- TOPLEVEL[f]
          n6 <- n5(PYCInt (100), PYCInt (1000), PYCInt (0))
          n7 <- TOPLEVEL[f]
          n8 <- n7(PYCInt (0), PYCInt (0), PYCInt (0), PYCString ("toto"))
          return PYCNone


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
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
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("TestHook")
          n1 <- TOPLEVEL[RuntimeError]
          TOPLEVEL[__init__] <- $FuncObj(__init__, dummy.TestHook.__init__, {(exc_type, n1); (raise_on_events, PYCNone); })
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
          n1 <- TOPLEVEL[C]
          n2 <- n1()
          TOPLEVEL[c] <- n2
          n3 <- TOPLEVEL[c]
          n4 <- $CallMethod($LoadMethod(n3, f), PYCInt (0))
          n5 <- TOPLEVEL[c]
          n6 <- $CallMethod($LoadMethod(n5, f), PYCInt (0), PYCInt (1))
          n7 <- TOPLEVEL[c]
          n8 <- $CallMethod($LoadMethod(n7, f), PYCInt (0), PYCInt (1), PYCInt (2))
          return PYCNone


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("C")
          TOPLEVEL[f] <- $FuncObj(f, dummy.C.f, {(y, PYCInt (1)); (z, PYCInt (10)); })
          return PYCNone


      dummy.C.f:
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[y]
          n2 <- $Binary.Add(n0, n1)
          n3 <- LOCAL[z]
          n4 <- $Binary.Add(n2, n3)
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
          n0 <- $BuildClass($FuncObj(C, dummy.C, {}), PYCString ("C"))
          TOPLEVEL[C] <- n0
          return PYCNone


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("C")
          $SETUP_ANNOTATIONS
          TOPLEVEL[x] <- PYCInt (0)
          n1 <- TOPLEVEL[int]
          n2 <- TOPLEVEL[__annotations__]
          n2[PYCString ("x")] <- n1
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
          n6 <- GLOBAL[KeyError]
          n7 <- $Compare.exception(n5, n6)
          if n7 then jmp b2(n5, n4, n3, n2, n1, n0) else jmp b3(n5, n4, n3, n2, n1, n0)

        b2:
          n20 <- LOCAL[self]
          n21 <- n20.default
          jmp b4(n21)

        b3:
          return PYCNone

        b4:
          return n22


      dummy.defaultdict:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
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
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("AsyncYieldFrom")
          TOPLEVEL[__await__] <- $FuncObj(__await__, dummy.AsyncYieldFrom.__await__, {})
          return PYCNone


      dummy.AsyncYieldFrom.__await__:
        b0:
          n0 <- LOCAL[self]
          n1 <- n0.obj
          n2 <- $GetYieldFromIter(n1)
          n3 <- $YieldFrom(n2, PYCNone)
          return PYCNone


      dummy.f:
        b0:
          n0 <- $Yield(PYCInt (42))
          return PYCNone


      dummy.powerset:
        b0:
          n0 <- GLOBAL[range]
          n1 <- GLOBAL[len]
          n2 <- LOCAL[s]
          n3 <- n1(n2)
          n4 <- $Binary.Add(n3, PYCInt (1))
          n5 <- n0(n4)
          n6 <- $GetIter(n5)
          jmp b1(n6)

        b1:
          n8 <- $NextIter(n7)
          n9 <- $HasNextIter(n8)
          if n9 then jmp b2 else jmp b3

        b2:
          n10 <- $IterData(n8)
          LOCAL[i] <- n10
          n11 <- GLOBAL[map]
          n12 <- GLOBAL[frozenset]
          n13 <- GLOBAL[itertools]
          n14 <- LOCAL[s]
          n15 <- LOCAL[i]
          n16 <- $CallMethod($LoadMethod(n13, combinations), n14, n15)
          n17 <- n11(n12, n16)
          n18 <- $GetYieldFromIter(n17)
          n19 <- $YieldFrom(n18, PYCNone)
          jmp b1(n7)

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
          n1 <- TOPLEVEL[C]
          n2 <- n1()
          TOPLEVEL[c] <- n2
          n3 <- $Delete(TOPLEVEL[c])()
          n4 <- TOPLEVEL[C]
          n5 <- n4()
          GLOBAL[c0] <- n5
          n6 <- GLOBAL[c0]
          n7 <- $DeleteAttr(foo)(n6)
          TOPLEVEL[f] <- $FuncObj(f, dummy.f, {})
          TOPLEVEL[g] <- $FuncObj(g, dummy.g, {})
          return PYCNone


      dummy.C:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
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
          n0 <- LOCAL[a]
          n1 <- LOCAL[b]
          n2 <- $DeleteSubscr(n0, n1)
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
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("C")
          TOPLEVEL[f] <- $FuncObj(f, dummy.C.f, {})
          return PYCNone


      dummy.C.f.D:
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- PYCString ("C.f.<locals>.D")
          n1 <- DEREF[binascii]
          n2 <- n1.unhexlify
          TOPLEVEL[g] <- $FuncObj(g, dummy.C.f.D.g, {(unhexlify, n2); })
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
