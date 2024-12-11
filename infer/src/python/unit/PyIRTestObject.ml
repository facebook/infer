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

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["C", "dummy.C", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "C", n0)
          TOPLEVEL[C] <- n4
          n5 <- TOPLEVEL[C]
          n6 <- $Call(n5, 0, "a", n0)
          TOPLEVEL[c] <- n6
          n7 <- TOPLEVEL[c]
          n8 <- n7.x
          n9 <- TOPLEVEL[c]
          n10 <- $CallMethod[get](n9, n0)
          n11 <- TOPLEVEL[c]
          n12 <- $CallMethod[set](n11, 42, n0)
          return n0


      function dummy.C():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C"
          n4 <- $MakeFunction["__init__", "dummy.C.__init__", n0, n0, n0, n0]
          TOPLEVEL[__init__] <- n4
          n5 <- $MakeFunction["get", "dummy.C.get", n0, n0, n0, n0]
          TOPLEVEL[get] <- n5
          n6 <- $MakeFunction["set", "dummy.C.set", n0, n0, n0, n0]
          TOPLEVEL[set] <- n6
          return n0


      function dummy.C.__init__(self, x, y):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          n4 <- LOCAL[self]
          n4.x <- n3
          n5 <- LOCAL[y]
          n6 <- LOCAL[self]
          n6.y <- n5
          return n0


      function dummy.C.get(self):
        b0:
          n0 <- None
          n3 <- LOCAL[self]
          n4 <- n3.x
          return n4


      function dummy.C.set(self, x):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          n4 <- LOCAL[self]
          n4.x <- n3
          return n0 |}]


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

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["IntBox", "dummy.IntBox", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "IntBox", n0)
          TOPLEVEL[IntBox] <- n4
          n5 <- TOPLEVEL[IntBox]
          n6 <- TOPLEVEL[int]
          n7 <- $MakeFunction["getX", "dummy.getX", n0, n0, $BuildTuple("box", n5, "return", n6), n0]
          TOPLEVEL[getX] <- n7
          n8 <- TOPLEVEL[IntBox]
          n9 <- $Call(n8, 10, n0)
          TOPLEVEL[c] <- n9
          n10 <- TOPLEVEL[c]
          n11 <- n10.x
          n12 <- TOPLEVEL[c]
          n12.z <- 10
          n13 <- TOPLEVEL[c]
          n14 <- $CallMethod[get](n13, n0)
          n15 <- TOPLEVEL[c]
          n16 <- $CallMethod[set](n15, 42, n0)
          n17 <- TOPLEVEL[c]
          n18 <- $CallMethod[run](n17, n0)
          n19 <- TOPLEVEL[print]
          n20 <- TOPLEVEL[c]
          n21 <- n20.z
          n22 <- $Call(n19, n21, n0)
          return n0


      function dummy.IntBox():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "IntBox"
          $SETUP_ANNOTATIONS
          n4 <- TOPLEVEL[int]
          n5 <- TOPLEVEL[__annotations__]
          n5["x"] <- n4
          n6 <- TOPLEVEL[int]
          n7 <- $MakeFunction["__init__", "dummy.IntBox.__init__", n0, n0, $BuildTuple("x", n6, "return", n0), n0]
          TOPLEVEL[__init__] <- n7
          n8 <- TOPLEVEL[int]
          n9 <- $MakeFunction["get", "dummy.IntBox.get", n0, n0, $BuildTuple("return", n8), n0]
          TOPLEVEL[get] <- n9
          n10 <- TOPLEVEL[int]
          n11 <- $MakeFunction["set", "dummy.IntBox.set", n0, n0, $BuildTuple("x", n10, "return", n0), n0]
          TOPLEVEL[set] <- n11
          n12 <- $MakeFunction["run", "dummy.IntBox.run", n0, n0, $BuildTuple("return", n0), n0]
          TOPLEVEL[run] <- n12
          n13 <- TOPLEVEL[staticmethod]
          n14 <- TOPLEVEL[int]
          n15 <- TOPLEVEL[int]
          n16 <- $MakeFunction["id", "dummy.IntBox.id", n0, n0, $BuildTuple("x", n14, "return", n15), n0]
          n17 <- $Call(n13, n16, n0)
          TOPLEVEL[id] <- n17
          return n0


      function dummy.IntBox.__init__(self, x):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          n4 <- LOCAL[self]
          n4.x <- n3
          return n0


      function dummy.IntBox.get(self):
        b0:
          n0 <- None
          n3 <- LOCAL[self]
          n4 <- n3.x
          return n4


      function dummy.IntBox.id(x):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          return n3


      function dummy.IntBox.run(self):
        b0:
          n0 <- None
          return n0


      function dummy.IntBox.set(self, x):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          n4 <- LOCAL[self]
          n4.x <- n3
          return n0


      function dummy.getX(box):
        b0:
          n0 <- None
          n3 <- LOCAL[box]
          n4 <- $CallMethod[get](n3, n0)
          return n4 |}]


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

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["C", "dummy.C", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "C", n0)
          TOPLEVEL[C] <- n4
          n5 <- $MakeFunction["D", "dummy.D", n0, n0, n0, n0]
          n6 <- TOPLEVEL[C]
          n7 <- $BuildClass(n5, "D", n6, n0)
          TOPLEVEL[D] <- n7
          return n0


      function dummy.C():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C"
          n4 <- TOPLEVEL[staticmethod]
          n5 <- $MakeFunction["f", "dummy.C.f", n0, n0, n0, n0]
          n6 <- $Call(n4, n5, n0)
          TOPLEVEL[f] <- n6
          n7 <- TOPLEVEL[staticmethod]
          n8 <- TOPLEVEL[int]
          n9 <- TOPLEVEL[int]
          n10 <- $MakeFunction["typed_f", "dummy.C.typed_f", n0, n0, $BuildTuple("x", n8, "return", n9), n0]
          n11 <- $Call(n7, n10, n0)
          TOPLEVEL[typed_f] <- n11
          return n0


      function dummy.C.f():
        b0:
          n0 <- None
          return n0


      function dummy.C.typed_f(x):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          return n3


      function dummy.D():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "D"
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

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["C", "dummy.C", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "C", n0)
          TOPLEVEL[C] <- n4
          n5 <- TOPLEVEL[C]
          n6 <- $CallMethod[f](n5, n0)
          return n0


      function dummy.C():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C"
          n4 <- TOPLEVEL[staticmethod]
          n5 <- $MakeFunction["f", "dummy.C.f", n0, n0, n0, n0]
          n6 <- $Call(n4, n5, n0)
          TOPLEVEL[f] <- n6
          return n0


      function dummy.C.f():
        b0:
          n0 <- None
          return n0 |}]


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

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["A", "dummy.A", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "A", n0)
          TOPLEVEL[A] <- n4
          n5 <- $MakeFunction["C", "dummy.C", n0, n0, n0, n0]
          n6 <- $BuildClass(n5, "C", n0)
          TOPLEVEL[C] <- n6
          n7 <- TOPLEVEL[C]
          n8 <- $MakeFunction["g", "dummy.g", n0, n0, $BuildTuple("c", n7, "return", n0), n0]
          TOPLEVEL[g] <- n8
          return n0


      function dummy.A():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "A"
          n4 <- $MakeFunction["f", "dummy.A.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n4
          return n0


      function dummy.A.f(self):
        b0:
          n0 <- None
          return n0


      function dummy.C():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C"
          $SETUP_ANNOTATIONS
          n4 <- TOPLEVEL[A]
          n5 <- TOPLEVEL[__annotations__]
          n5["a"] <- n4
          return n0


      function dummy.g(c):
        b0:
          n0 <- None
          n3 <- GLOBAL[print]
          n4 <- LOCAL[c]
          n5 <- n4.a
          n6 <- $CallMethod[f](n5, n0)
          n7 <- $Call(n3, n6, n0)
          return n0 |}]


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

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["A", "dummy.A", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "A", n0)
          TOPLEVEL[A] <- n4
          n5 <- $MakeFunction["B", "dummy.B", n0, n0, n0, n0]
          n6 <- $BuildClass(n5, "B", n0)
          TOPLEVEL[B] <- n6
          n7 <- $MakeFunction["C", "dummy.C", n0, n0, n0, n0]
          n8 <- TOPLEVEL[A]
          n9 <- TOPLEVEL[B]
          n10 <- $BuildClass(n7, "C", n8, n9, n0)
          TOPLEVEL[C] <- n10
          return n0


      function dummy.A():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "A"
          return n0


      function dummy.B():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "B"
          return n0


      function dummy.C():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C"
          return n0 |}]


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

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["C", "dummy.C", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "C", n0)
          TOPLEVEL[C] <- n4
          n5 <- $MakeFunction["build", "dummy.build", n0, n0, n0, n0]
          TOPLEVEL[build] <- n5
          n6 <- TOPLEVEL[build]
          n7 <- $Call(n6, n0)
          TOPLEVEL[cs] <- n7
          n8 <- TOPLEVEL[cs]
          n9 <- n8[0]
          n10 <- n9.x
          return n0


      function dummy.C():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C"
          n4 <- $MakeFunction["__init__", "dummy.C.__init__", n0, n0, n0, n0]
          TOPLEVEL[__init__] <- n4
          return n0


      function dummy.C.__init__(self):
        b0:
          n0 <- None
          n3 <- LOCAL[self]
          n3.x <- 0
          return n0


      function dummy.build():
        b0:
          n0 <- None
          n3 <- GLOBAL[C]
          n4 <- $Call(n3, n0)
          return $BuildList(n4) |}]


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

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["f", "dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n3
          n4 <- TOPLEVEL[f]
          n5 <- $Call(n4, n0)
          return n0


      function dummy.f(A, a):
        b0:
          n0 <- None
          n3 <- $MakeFunction["A", "dummy.f.A", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "A", n0)
          LOCAL[A] <- n4
          n5 <- LOCAL[A]
          n6 <- $Call(n5, n0)
          LOCAL[a] <- n6
          n7 <- LOCAL[a]
          n8 <- $CallMethod[get](n7, n0)
          return n8


      function dummy.f.A():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "f.<locals>.A"
          n4 <- $MakeFunction["__init__", "dummy.f.A.__init__", n0, n0, n0, n0]
          TOPLEVEL[__init__] <- n4
          n5 <- $MakeFunction["get", "dummy.f.A.get", n0, n0, n0, n0]
          TOPLEVEL[get] <- n5
          return n0


      function dummy.f.A.__init__(self):
        b0:
          n0 <- None
          n3 <- LOCAL[self]
          n3.x <- 0
          return n0


      function dummy.f.A.get(self):
        b0:
          n0 <- None
          n3 <- LOCAL[self]
          n4 <- n3.x
          return n4 |}]


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

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["C", "dummy.C", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "C", n0)
          TOPLEVEL[C] <- n4
          n5 <- $MakeFunction["D", "dummy.D", n0, n0, n0, n0]
          n6 <- TOPLEVEL[C]
          n7 <- $BuildClass(n5, "D", n6, n0)
          TOPLEVEL[D] <- n7
          return n0


      function dummy.C():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C"
          return n0


      function dummy.D():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "D"
          return n0 |}]


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

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["C", "dummy.C", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "C", n0)
          TOPLEVEL[C] <- n4
          n5 <- $MakeFunction["D", "dummy.D", n0, n0, n0, n0]
          n6 <- TOPLEVEL[C]
          n7 <- $BuildClass(n5, "D", n6, n0)
          TOPLEVEL[D] <- n7
          n8 <- $MakeFunction["C0", "dummy.C0", n0, n0, n0, n0]
          n9 <- $BuildClass(n8, "C0", n0)
          TOPLEVEL[C0] <- n9
          n10 <- $MakeFunction["D0", "dummy.D0", n0, n0, n0, n0]
          n11 <- TOPLEVEL[C0]
          n12 <- $BuildClass(n10, "D0", n11, n0)
          TOPLEVEL[D0] <- n12
          return n0


      function dummy.C():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C"
          return n0


      function dummy.C0():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C0"
          n4 <- $MakeFunction["__init__", "dummy.C0.__init__", n0, n0, n0, n0]
          TOPLEVEL[__init__] <- n4
          return n0


      function dummy.C0.__init__(foo, x):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          n4 <- LOCAL[foo]
          n4.x <- n3
          return n0


      function dummy.D():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "D"
          n4 <- $LoadClosure(0,"__class__")
          n5 <- $MakeFunction["__init__", "dummy.D.__init__", n0, n0, n0, $BuildTuple(n4)]
          TOPLEVEL[__init__] <- n5
          n6 <- $LoadClosure(0,"__class__")
          TOPLEVEL[__classcell__] <- n6
          return n6


      function dummy.D.__init__(self):
        b0:
          n0 <- None
          n3 <- GLOBAL[super]
          n4 <- $Call(n3, n0)
          n5 <- $CallMethod[__init__](n4, n0)
          return n0


      function dummy.D0():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "D0"
          n4 <- $LoadClosure(0,"__class__")
          n5 <- $MakeFunction["__init__", "dummy.D0.__init__", n0, n0, n0, $BuildTuple(n4)]
          TOPLEVEL[__init__] <- n5
          n6 <- $LoadClosure(0,"__class__")
          TOPLEVEL[__classcell__] <- n6
          return n6


      function dummy.D0.__init__(bar):
        b0:
          n0 <- None
          n3 <- GLOBAL[super]
          n4 <- $Call(n3, n0)
          n5 <- $CallMethod[__init__](n4, 42, n0)
          return n0 |}]


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

      function toplevel():
        b0:
          n0 <- None
          n3 <- $ImportName(foo, n0, 0)
          TOPLEVEL[foo] <- n3
          n4 <- $MakeFunction["C", "dummy.C", n0, n0, n0, n0]
          n5 <- TOPLEVEL[foo]
          n6 <- n5.D
          n7 <- $BuildClass(n4, "C", n6, n0)
          TOPLEVEL[C] <- n7
          return n0


      function dummy.C():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C"
          n4 <- $LoadClosure(0,"__class__")
          n5 <- $MakeFunction["__init__", "dummy.C.__init__", n0, n0, n0, $BuildTuple(n4)]
          TOPLEVEL[__init__] <- n5
          n6 <- $LoadClosure(0,"__class__")
          TOPLEVEL[__classcell__] <- n6
          return n6


      function dummy.C.__init__(self, x):
        b0:
          n0 <- None
          n3 <- GLOBAL[super]
          n4 <- $Call(n3, n0)
          n5 <- LOCAL[x]
          n6 <- $CallMethod[__init__](n4, n5, n0)
          return n0 |}]


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

      function toplevel():
        b0:
          n0 <- None
          n3 <- $ImportName(abc, $BuildTuple("ABC", "abstractmethod"), 0)
          n4 <- $ImportFrom(ABC, n3)
          TOPLEVEL[ABC] <- n4
          n5 <- $ImportFrom(abstractmethod, n3)
          TOPLEVEL[abstractmethod] <- n5
          n6 <- $MakeFunction["C", "dummy.C", n0, n0, n0, n0]
          n7 <- TOPLEVEL[ABC]
          n8 <- $BuildClass(n6, "C", n7, n0)
          TOPLEVEL[C] <- n8
          return n0


      function dummy.C():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C"
          n4 <- TOPLEVEL[abstractmethod]
          n5 <- $MakeFunction["get", "dummy.C.get", n0, n0, $BuildTuple("return", n0), n0]
          n6 <- $Call(n4, n5, n0)
          TOPLEVEL[get] <- n6
          n7 <- TOPLEVEL[abstractmethod]
          n8 <- TOPLEVEL[staticmethod]
          n9 <- $MakeFunction["get_static0", "dummy.C.get_static0", n0, n0, $BuildTuple("return", n0), n0]
          n10 <- $Call(n8, n9, n0)
          n11 <- $Call(n7, n10, n0)
          TOPLEVEL[get_static0] <- n11
          n12 <- TOPLEVEL[staticmethod]
          n13 <- TOPLEVEL[abstractmethod]
          n14 <- $MakeFunction["get_static1", "dummy.C.get_static1", n0, n0, $BuildTuple("return", n0), n0]
          n15 <- $Call(n13, n14, n0)
          n16 <- $Call(n12, n15, n0)
          TOPLEVEL[get_static1] <- n16
          return n0


      function dummy.C.get(self):
        b0:
          n0 <- None
          return n0


      function dummy.C.get_static0():
        b0:
          n0 <- None
          return n0


      function dummy.C.get_static1():
        b0:
          n0 <- None
          return n0 |}]


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

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["C", "dummy.C", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "C", n0)
          TOPLEVEL[C] <- n4
          return n0


      function dummy.C():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C"
          n4 <- TOPLEVEL[foo]
          n5 <- TOPLEVEL[x]
          n6 <- TOPLEVEL[y]
          n7 <- TOPLEVEL[z]
          n8 <- $Call(n4, n5, n6, n7, n0)
          n9 <- $MakeFunction["f", "dummy.C.f", n0, n0, n0, n0]
          n10 <- $Call(n8, n9, n0)
          TOPLEVEL[f] <- n10
          n11 <- TOPLEVEL[foo]
          n12 <- TOPLEVEL[x]
          n13 <- TOPLEVEL[y]
          n14 <- TOPLEVEL[z]
          n15 <- $CallMethod[bar](n11, n12, n13, n14, n0)
          n16 <- $MakeFunction["g", "dummy.C.g", n0, n0, n0, n0]
          n17 <- $Call(n15, n16, n0)
          TOPLEVEL[g] <- n17
          return n0


      function dummy.C.f(self):
        b0:
          n0 <- None
          return n0


      function dummy.C.g(self):
        b0:
          n0 <- None
          return n0 |}]


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

      function toplevel():
        b0:
          n0 <- None
          n3 <- $ImportName(unittest, n0, 0)
          TOPLEVEL[unittest] <- n3
          n4 <- $MakeFunction["PwdTest", "dummy.PwdTest", n0, n0, n0, n0]
          n5 <- TOPLEVEL[unittest]
          n6 <- n5.TestCase
          n7 <- $BuildClass(n4, "PwdTest", n6, n0)
          TOPLEVEL[PwdTest] <- n7
          return n0


      function dummy.PwdTest():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "PwdTest"
          n4 <- $MakeFunction["test_values", "dummy.PwdTest.test_values", n0, n0, n0, n0]
          TOPLEVEL[test_values] <- n4
          return n0


      function dummy.PwdTest.test_values(self, e):
        b0:
          n0 <- None
          n3 <- LOCAL[self]
          n4 <- GLOBAL[type]
          n5 <- LOCAL[e]
          n6 <- n5.pw_gecos
          n7 <- $Call(n4, n6, n0)
          n8 <- GLOBAL[str]
          n9 <- GLOBAL[type]
          n10 <- $Call(n9, n0, n0)
          n11 <- $CallMethod[assertIn](n3, n7, $BuildTuple(n8, n10), n0)
          return n0 |}]


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

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["C", "dummy.C", n0, n0, n0, n0]
          n4 <- TOPLEVEL[Exception]
          n5 <- $BuildClass(n3, "C", n4, n0)
          TOPLEVEL[C] <- n5
          n6 <- $MakeFunction["f", "dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n6
          n7 <- $MakeFunction["g", "dummy.g", n0, n0, n0, n0]
          TOPLEVEL[g] <- n7
          return n0


      function dummy.C():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C"
          return n0


      function dummy.f():
        b0:
          n0 <- None
          n3 <- GLOBAL[C]
          throw n3


      function dummy.g():
        b0:
          n0 <- None
          n3 <- GLOBAL[C]
          n4 <- $Call(n3, n0)
          throw n4 |}]


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

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["C", "dummy.C", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "C", n0)
          TOPLEVEL[C] <- n4
          n5 <- $MakeFunction["f", "dummy.f", $BuildTuple(1, 2, "zuck"), n0, n0, n0]
          TOPLEVEL[f] <- n5
          n6 <- TOPLEVEL[f]
          n7 <- $Call(n6, 0, n0)
          n8 <- TOPLEVEL[f]
          n9 <- $Call(n8, 10, 100, n0)
          n10 <- TOPLEVEL[f]
          n11 <- $Call(n10, 100, 1000, 0, n0)
          n12 <- TOPLEVEL[f]
          n13 <- $Call(n12, 0, 0, 0, "toto", n0)
          return n0


      function dummy.C():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C"
          return n0


      function dummy.f(x, y, z, s):
        b0:
          n0 <- None
          return n0 |}]


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

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["TestHook", "dummy.TestHook", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "TestHook", n0)
          TOPLEVEL[TestHook] <- n4
          return n0


      function dummy.TestHook():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "TestHook"
          n4 <- TOPLEVEL[RuntimeError]
          n5 <- $MakeFunction["__init__", "dummy.TestHook.__init__", $BuildTuple(n0, n4), n0, n0, n0]
          TOPLEVEL[__init__] <- n5
          return n0


      function dummy.TestHook.__init__(self, raise_on_events, exc_type):
        b0:
          n0 <- None
          return n0 |}]


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

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["C", "dummy.C", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "C", n0)
          TOPLEVEL[C] <- n4
          n5 <- TOPLEVEL[C]
          n6 <- $Call(n5, n0)
          TOPLEVEL[c] <- n6
          n7 <- TOPLEVEL[c]
          n8 <- $CallMethod[f](n7, 0, n0)
          n9 <- TOPLEVEL[c]
          n10 <- $CallMethod[f](n9, 0, 1, n0)
          n11 <- TOPLEVEL[c]
          n12 <- $CallMethod[f](n11, 0, 1, 2, n0)
          return n0


      function dummy.C():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C"
          n4 <- $MakeFunction["f", "dummy.C.f", $BuildTuple(1, 10), n0, n0, n0]
          TOPLEVEL[f] <- n4
          return n0


      function dummy.C.f(self, x, y, z):
        b0:
          n0 <- None
          n3 <- LOCAL[x]
          n4 <- LOCAL[y]
          n5 <- $Binary.Add(n3, n4, n0)
          n6 <- LOCAL[z]
          n7 <- $Binary.Add(n5, n6, n0)
          return n7 |}]


let%expect_test _ =
  let source = {|
class C:
        x : int = 0
        |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["C", "dummy.C", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "C", n0)
          TOPLEVEL[C] <- n4
          return n0


      function dummy.C():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C"
          $SETUP_ANNOTATIONS
          TOPLEVEL[x] <- 0
          n4 <- TOPLEVEL[int]
          n5 <- TOPLEVEL[__annotations__]
          n5["x"] <- n4
          return n0 |}]


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

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["defaultdict", "dummy.defaultdict", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "defaultdict", n0)
          TOPLEVEL[defaultdict] <- n4
          return n0


      function dummy.defaultdict():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "defaultdict"
          n4 <- $MakeFunction["__getitem__", "dummy.defaultdict.__getitem__", n0, n0, n0, n0]
          TOPLEVEL[__getitem__] <- n4
          return n0


      function dummy.defaultdict.__getitem__(self, key):
        b0:
          n0 <- None
          return 42 |}]


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

      function toplevel():
        b0:
          n0 <- None
          n3 <- $ImportName(itertools, n0, 0)
          TOPLEVEL[itertools] <- n3
          n4 <- $MakeFunction["f", "dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n4
          n5 <- $MakeFunction["AsyncYieldFrom", "dummy.AsyncYieldFrom", n0, n0, n0, n0]
          n6 <- $BuildClass(n5, "AsyncYieldFrom", n0)
          TOPLEVEL[AsyncYieldFrom] <- n6
          n7 <- $MakeFunction["powerset", "dummy.powerset", n0, n0, n0, n0]
          TOPLEVEL[powerset] <- n7
          return n0


      function dummy.AsyncYieldFrom():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "AsyncYieldFrom"
          n4 <- $MakeFunction["__await__", "dummy.AsyncYieldFrom.__await__", n0, n0, n0, n0]
          TOPLEVEL[__await__] <- n4
          return n0


      function dummy.AsyncYieldFrom.__await__(self):
        b0:
          n0 <- None
          $GenStartGenerator()
          n3 <- LOCAL[self]
          n4 <- n3.obj
          n5 <- $GetYieldFromIter(n4, n0)
          n6 <- $YieldFrom(n5, n0, n0)
          return n0


      function dummy.f():
        b0:
          n0 <- None
          $GenStartGenerator()
          n3 <- $Yield(42)
          return n0


      function dummy.powerset(s):
        b0:
          n0 <- None
          $GenStartGenerator()
          n3 <- GLOBAL[range]
          n4 <- GLOBAL[len]
          n5 <- LOCAL[s]
          n6 <- $Call(n4, n5, n0)
          n7 <- $Binary.Add(n6, 1, n0)
          n8 <- $Call(n3, n7, n0)
          n9 <- $GetIter(n8, n0)
          jmp b1

        b1:
          n10 <- $NextIter(n9, n0)
          n11 <- $HasNextIter(n9, n0)
          if n11 then jmp b2 else jmp b3

        b2:
          LOCAL[i] <- n10
          n12 <- GLOBAL[map]
          n13 <- GLOBAL[frozenset]
          n14 <- GLOBAL[itertools]
          n15 <- LOCAL[s]
          n16 <- LOCAL[i]
          n17 <- $CallMethod[combinations](n14, n15, n16, n0)
          n18 <- $Call(n12, n13, n17, n0)
          n19 <- $GetYieldFromIter(n18, n0)
          n20 <- $YieldFrom(n19, n0, n0)
          jmp b1

        b3:
          return n0 |}]


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

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["C", "dummy.C", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "C", n0)
          TOPLEVEL[C] <- n4
          n5 <- TOPLEVEL[C]
          n6 <- $Call(n5, n0)
          TOPLEVEL[c] <- n6
          $Delete(TOPLEVEL[c])
          n7 <- TOPLEVEL[C]
          n8 <- $Call(n7, n0)
          GLOBAL[c0] <- n8
          n9 <- GLOBAL[c0]
          $DeleteAttr(n9, foo)
          n10 <- $MakeFunction["f", "dummy.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n10
          n11 <- $MakeFunction["g", "dummy.g", n0, n0, n0, n0]
          TOPLEVEL[g] <- n11
          return n0


      function dummy.C():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C"
          return n0


      function dummy.f(x):
        b0:
          n0 <- None
          $Delete(GLOBAL[c0])
          $Delete(LOCAL[x])
          $StoreDeref(0,"z", 0)
          n3 <- $LoadClosure(0,"z")
          n4 <- $MakeFunction["inner", "dummy.f.inner", n0, n0, n0, $BuildTuple(n3)]
          LOCAL[inner] <- n4
          return n0


      function dummy.f.inner():
        b0:
          n0 <- None
          n3 <- GLOBAL[print]
          n4 <- $LoadDeref(0,"z")
          n5 <- $Call(n3, n4, n0)
          $DeleteDeref[0,"z")
          return n0


      function dummy.g(a, b):
        b0:
          n0 <- None
          n3 <- LOCAL[a]
          n4 <- LOCAL[b]
          n5 <- $DeleteSubscr(n3, n4, n0)
          return n0 |}]


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

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["C", "dummy.C", n0, n0, n0, n0]
          n4 <- $BuildClass(n3, "C", n0)
          TOPLEVEL[C] <- n4
          return n0


      function dummy.C():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C"
          n4 <- $MakeFunction["f", "dummy.C.f", n0, n0, n0, n0]
          TOPLEVEL[f] <- n4
          return n0


      function dummy.C.f(self):
        b0:
          n0 <- None
          n3 <- $ImportName(binascii, n0, 0)
          $StoreDeref(0,"binascii", n3)
          n4 <- $LoadClosure(0,"binascii")
          n5 <- $MakeFunction["D", "dummy.C.f.D", n0, n0, n0, $BuildTuple(n4)]
          n6 <- $BuildClass(n5, "D", n0)
          LOCAL[D] <- n6
          return n0


      function dummy.C.f.D():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C.f.<locals>.D"
          n4 <- $LoadClassDeref(0,"binascii")
          n5 <- n4.unhexlify
          n6 <- $MakeFunction["g", "dummy.C.f.D.g", $BuildTuple(n5), n0, n0, n0]
          TOPLEVEL[g] <- n6
          return n0


      function dummy.C.f.D.g(self, unhexlify):
        b0:
          n0 <- None
          return n0 |}]


let%expect_test _ =
  let source = {|
class C(metaclass=m):
    pass
        |} in
  PyIR.test source ;
  [%expect
    {|
    module dummy:

      function toplevel():
        b0:
          n0 <- None
          n3 <- $MakeFunction["C", "dummy.C", n0, n0, n0, n0]
          n4 <- TOPLEVEL[m]
          n5 <- $BuildClass(n3, "C", n4, $BuildTuple("metaclass"))
          TOPLEVEL[C] <- n5
          return n0


      function dummy.C():
        b0:
          n0 <- None
          n3 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n3
          TOPLEVEL[__qualname__] <- "C"
          return n0 |}]
