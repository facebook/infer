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
          n0 <- $MakeFunction["C", "dummy.C", None, None, None, None]
          n1 <- $BuildClass(n0, "C", None)
          TOPLEVEL[C] <- n1
          n2 <- TOPLEVEL[C]
          n3 <- $Call(n2, 0, "a", None)
          TOPLEVEL[c] <- n3
          n4 <- TOPLEVEL[c]
          n5 <- n4.x
          n6 <- TOPLEVEL[c]
          n7 <- $CallMethod[get](n6, None)
          n8 <- TOPLEVEL[c]
          n9 <- $CallMethod[set](n8, 42, None)
          return None


      function dummy.C():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          n1 <- $MakeFunction["__init__", "dummy.C.__init__", None, None, None, None]
          TOPLEVEL[__init__] <- n1
          n2 <- $MakeFunction["get", "dummy.C.get", None, None, None, None]
          TOPLEVEL[get] <- n2
          n3 <- $MakeFunction["set", "dummy.C.set", None, None, None, None]
          TOPLEVEL[set] <- n3
          return None


      function dummy.C.__init__(self, x, y):
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[self]
          n1.x <- n0
          n2 <- LOCAL[y]
          n3 <- LOCAL[self]
          n3.y <- n2
          return None


      function dummy.C.get(self):
        b0:
          n0 <- LOCAL[self]
          n1 <- n0.x
          return n1


      function dummy.C.set(self, x):
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

      function toplevel():
        b0:
          n0 <- $MakeFunction["IntBox", "dummy.IntBox", None, None, None, None]
          n1 <- $BuildClass(n0, "IntBox", None)
          TOPLEVEL[IntBox] <- n1
          n2 <- TOPLEVEL[IntBox]
          n3 <- TOPLEVEL[int]
          n4 <- $BuildConstKeyMap($BuildTuple("box", "return"), n2, n3, None)
          n5 <- $MakeFunction["getX", "dummy.getX", None, None, n4, None]
          TOPLEVEL[getX] <- n5
          n6 <- TOPLEVEL[IntBox]
          n7 <- $Call(n6, 10, None)
          TOPLEVEL[c] <- n7
          n8 <- TOPLEVEL[c]
          n9 <- n8.x
          n10 <- TOPLEVEL[c]
          n10.z <- 10
          n11 <- TOPLEVEL[c]
          n12 <- $CallMethod[get](n11, None)
          n13 <- TOPLEVEL[c]
          n14 <- $CallMethod[set](n13, 42, None)
          n15 <- TOPLEVEL[c]
          n16 <- $CallMethod[run](n15, None)
          n17 <- TOPLEVEL[print]
          n18 <- TOPLEVEL[c]
          n19 <- n18.z
          n20 <- $Call(n17, n19, None)
          return None


      function dummy.IntBox():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "IntBox"
          $SETUP_ANNOTATIONS
          n1 <- TOPLEVEL[int]
          n2 <- TOPLEVEL[__annotations__]
          n2["x"] <- n1
          n3 <- TOPLEVEL[int]
          n4 <- $BuildConstKeyMap($BuildTuple("x", "return"), n3, None, None)
          n5 <- $MakeFunction["__init__", "dummy.IntBox.__init__", None, None, n4, None]
          TOPLEVEL[__init__] <- n5
          n6 <- TOPLEVEL[int]
          n7 <- $BuildConstKeyMap($BuildTuple("return"), n6, None)
          n8 <- $MakeFunction["get", "dummy.IntBox.get", None, None, n7, None]
          TOPLEVEL[get] <- n8
          n9 <- TOPLEVEL[int]
          n10 <- $BuildConstKeyMap($BuildTuple("x", "return"), n9, None, None)
          n11 <- $MakeFunction["set", "dummy.IntBox.set", None, None, n10, None]
          TOPLEVEL[set] <- n11
          n12 <- $BuildConstKeyMap($BuildTuple("return"), None, None)
          n13 <- $MakeFunction["run", "dummy.IntBox.run", None, None, n12, None]
          TOPLEVEL[run] <- n13
          n14 <- TOPLEVEL[staticmethod]
          n15 <- TOPLEVEL[int]
          n16 <- TOPLEVEL[int]
          n17 <- $BuildConstKeyMap($BuildTuple("x", "return"), n15, n16, None)
          n18 <- $MakeFunction["id", "dummy.IntBox.id", None, None, n17, None]
          n19 <- $Call(n14, n18, None)
          TOPLEVEL[id] <- n19
          return None


      function dummy.IntBox.__init__(self, x):
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[self]
          n1.x <- n0
          return None


      function dummy.IntBox.get(self):
        b0:
          n0 <- LOCAL[self]
          n1 <- n0.x
          return n1


      function dummy.getX(box):
        b0:
          n0 <- LOCAL[box]
          n1 <- $CallMethod[get](n0, None)
          return n1


      function dummy.IntBox.id(x):
        b0:
          n0 <- LOCAL[x]
          return n0


      function dummy.IntBox.run(self):
        b0:
          return None


      function dummy.IntBox.set(self, x):
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

      function toplevel():
        b0:
          n0 <- $MakeFunction["C", "dummy.C", None, None, None, None]
          n1 <- $BuildClass(n0, "C", None)
          TOPLEVEL[C] <- n1
          n2 <- $MakeFunction["D", "dummy.D", None, None, None, None]
          n3 <- TOPLEVEL[C]
          n4 <- $BuildClass(n2, "D", n3, None)
          TOPLEVEL[D] <- n4
          return None


      function dummy.C():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          n1 <- TOPLEVEL[staticmethod]
          n2 <- $MakeFunction["f", "dummy.C.f", None, None, None, None]
          n3 <- $Call(n1, n2, None)
          TOPLEVEL[f] <- n3
          n4 <- TOPLEVEL[staticmethod]
          n5 <- TOPLEVEL[int]
          n6 <- TOPLEVEL[int]
          n7 <- $BuildConstKeyMap($BuildTuple("x", "return"), n5, n6, None)
          n8 <- $MakeFunction["typed_f", "dummy.C.typed_f", None, None, n7, None]
          n9 <- $Call(n4, n8, None)
          TOPLEVEL[typed_f] <- n9
          return None


      function dummy.D():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "D"
          return None


      function dummy.C.f():
        b0:
          return None


      function dummy.C.typed_f(x):
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

      function toplevel():
        b0:
          n0 <- $MakeFunction["C", "dummy.C", None, None, None, None]
          n1 <- $BuildClass(n0, "C", None)
          TOPLEVEL[C] <- n1
          n2 <- TOPLEVEL[C]
          n3 <- $CallMethod[f](n2, None)
          return None


      function dummy.C():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          n1 <- TOPLEVEL[staticmethod]
          n2 <- $MakeFunction["f", "dummy.C.f", None, None, None, None]
          n3 <- $Call(n1, n2, None)
          TOPLEVEL[f] <- n3
          return None


      function dummy.C.f():
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

      function toplevel():
        b0:
          n0 <- $MakeFunction["A", "dummy.A", None, None, None, None]
          n1 <- $BuildClass(n0, "A", None)
          TOPLEVEL[A] <- n1
          n2 <- $MakeFunction["C", "dummy.C", None, None, None, None]
          n3 <- $BuildClass(n2, "C", None)
          TOPLEVEL[C] <- n3
          n4 <- TOPLEVEL[C]
          n5 <- $BuildConstKeyMap($BuildTuple("c", "return"), n4, None, None)
          n6 <- $MakeFunction["g", "dummy.g", None, None, n5, None]
          TOPLEVEL[g] <- n6
          return None


      function dummy.A():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "A"
          n1 <- $MakeFunction["f", "dummy.A.f", None, None, None, None]
          TOPLEVEL[f] <- n1
          return None


      function dummy.C():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          $SETUP_ANNOTATIONS
          n1 <- TOPLEVEL[A]
          n2 <- TOPLEVEL[__annotations__]
          n2["a"] <- n1
          return None


      function dummy.A.f(self):
        b0:
          return None


      function dummy.g(c):
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

      function toplevel():
        b0:
          n0 <- $MakeFunction["A", "dummy.A", None, None, None, None]
          n1 <- $BuildClass(n0, "A", None)
          TOPLEVEL[A] <- n1
          n2 <- $MakeFunction["B", "dummy.B", None, None, None, None]
          n3 <- $BuildClass(n2, "B", None)
          TOPLEVEL[B] <- n3
          n4 <- $MakeFunction["C", "dummy.C", None, None, None, None]
          n5 <- TOPLEVEL[A]
          n6 <- TOPLEVEL[B]
          n7 <- $BuildClass(n4, "C", n5, n6, None)
          TOPLEVEL[C] <- n7
          return None


      function dummy.A():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "A"
          return None


      function dummy.B():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "B"
          return None


      function dummy.C():
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

      function toplevel():
        b0:
          n0 <- $MakeFunction["C", "dummy.C", None, None, None, None]
          n1 <- $BuildClass(n0, "C", None)
          TOPLEVEL[C] <- n1
          n2 <- $MakeFunction["build", "dummy.build", None, None, None, None]
          TOPLEVEL[build] <- n2
          n3 <- TOPLEVEL[build]
          n4 <- $Call(n3, None)
          TOPLEVEL[cs] <- n4
          n5 <- TOPLEVEL[cs]
          n6 <- n5[0]
          n7 <- n6.x
          return None


      function dummy.C():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          n1 <- $MakeFunction["__init__", "dummy.C.__init__", None, None, None, None]
          TOPLEVEL[__init__] <- n1
          return None


      function dummy.C.__init__(self):
        b0:
          n0 <- LOCAL[self]
          n0.x <- 0
          return None


      function dummy.build():
        b0:
          n0 <- GLOBAL[C]
          n1 <- $Call(n0, None)
          return $BuildList(n1) |}]


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
          n0 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n0
          n1 <- TOPLEVEL[f]
          n2 <- $Call(n1, None)
          return None


      function dummy.f.A():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "f.<locals>.A"
          n1 <- $MakeFunction["__init__", "dummy.f.A.__init__", None, None, None, None]
          TOPLEVEL[__init__] <- n1
          n2 <- $MakeFunction["get", "dummy.f.A.get", None, None, None, None]
          TOPLEVEL[get] <- n2
          return None


      function dummy.f.A.__init__(self):
        b0:
          n0 <- LOCAL[self]
          n0.x <- 0
          return None


      function dummy.f(A, a):
        b0:
          n0 <- $MakeFunction["A", "dummy.f.A", None, None, None, None]
          n1 <- $BuildClass(n0, "A", None)
          LOCAL[A] <- n1
          n2 <- LOCAL[A]
          n3 <- $Call(n2, None)
          LOCAL[a] <- n3
          n4 <- LOCAL[a]
          n5 <- $CallMethod[get](n4, None)
          return n5


      function dummy.f.A.get(self):
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

      function toplevel():
        b0:
          n0 <- $MakeFunction["C", "dummy.C", None, None, None, None]
          n1 <- $BuildClass(n0, "C", None)
          TOPLEVEL[C] <- n1
          n2 <- $MakeFunction["D", "dummy.D", None, None, None, None]
          n3 <- TOPLEVEL[C]
          n4 <- $BuildClass(n2, "D", n3, None)
          TOPLEVEL[D] <- n4
          return None


      function dummy.C():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          return None


      function dummy.D():
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

      function toplevel():
        b0:
          n0 <- $MakeFunction["C", "dummy.C", None, None, None, None]
          n1 <- $BuildClass(n0, "C", None)
          TOPLEVEL[C] <- n1
          n2 <- $MakeFunction["D", "dummy.D", None, None, None, None]
          n3 <- TOPLEVEL[C]
          n4 <- $BuildClass(n2, "D", n3, None)
          TOPLEVEL[D] <- n4
          n5 <- $MakeFunction["C0", "dummy.C0", None, None, None, None]
          n6 <- $BuildClass(n5, "C0", None)
          TOPLEVEL[C0] <- n6
          n7 <- $MakeFunction["D0", "dummy.D0", None, None, None, None]
          n8 <- TOPLEVEL[C0]
          n9 <- $BuildClass(n7, "D0", n8, None)
          TOPLEVEL[D0] <- n9
          return None


      function dummy.C():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          return None


      function dummy.C0():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C0"
          n1 <- $MakeFunction["__init__", "dummy.C0.__init__", None, None, None, None]
          TOPLEVEL[__init__] <- n1
          return None


      function dummy.D():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "D"
          n1 <- $LoadClosure(0,"__class__")
          n2 <- $MakeFunction["__init__", "dummy.D.__init__", None, None, None, $BuildTuple(n1)]
          TOPLEVEL[__init__] <- n2
          n3 <- $LoadClosure(0,"__class__")
          TOPLEVEL[__classcell__] <- n3
          return n3


      function dummy.D0():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "D0"
          n1 <- $LoadClosure(0,"__class__")
          n2 <- $MakeFunction["__init__", "dummy.D0.__init__", None, None, None, $BuildTuple(n1)]
          TOPLEVEL[__init__] <- n2
          n3 <- $LoadClosure(0,"__class__")
          TOPLEVEL[__classcell__] <- n3
          return n3


      function dummy.C0.__init__(foo, x):
        b0:
          n0 <- LOCAL[x]
          n1 <- LOCAL[foo]
          n1.x <- n0
          return None


      function dummy.D.__init__(self):
        b0:
          n0 <- GLOBAL[super]
          n1 <- $Call(n0, None)
          n2 <- $CallMethod[__init__](n1, None)
          return None


      function dummy.D0.__init__(bar):
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

      function toplevel():
        b0:
          n0 <- $ImportName(foo, None, 0)
          TOPLEVEL[foo] <- n0
          n1 <- $MakeFunction["C", "dummy.C", None, None, None, None]
          n2 <- TOPLEVEL[foo]
          n3 <- n2.D
          n4 <- $BuildClass(n1, "C", n3, None)
          TOPLEVEL[C] <- n4
          return None


      function dummy.C():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          n1 <- $LoadClosure(0,"__class__")
          n2 <- $MakeFunction["__init__", "dummy.C.__init__", None, None, None, $BuildTuple(n1)]
          TOPLEVEL[__init__] <- n2
          n3 <- $LoadClosure(0,"__class__")
          TOPLEVEL[__classcell__] <- n3
          return n3


      function dummy.C.__init__(self, x):
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

      function toplevel():
        b0:
          n0 <- $ImportName(abc, $BuildTuple("ABC", "abstractmethod"), 0)
          n1 <- $ImportFrom(ABC, n0)
          TOPLEVEL[ABC] <- n1
          n2 <- $ImportFrom(abstractmethod, n0)
          TOPLEVEL[abstractmethod] <- n2
          n3 <- $MakeFunction["C", "dummy.C", None, None, None, None]
          n4 <- TOPLEVEL[ABC]
          n5 <- $BuildClass(n3, "C", n4, None)
          TOPLEVEL[C] <- n5
          return None


      function dummy.C():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          n1 <- TOPLEVEL[abstractmethod]
          n2 <- $BuildConstKeyMap($BuildTuple("return"), None, None)
          n3 <- $MakeFunction["get", "dummy.C.get", None, None, n2, None]
          n4 <- $Call(n1, n3, None)
          TOPLEVEL[get] <- n4
          n5 <- TOPLEVEL[abstractmethod]
          n6 <- TOPLEVEL[staticmethod]
          n7 <- $BuildConstKeyMap($BuildTuple("return"), None, None)
          n8 <- $MakeFunction["get_static0", "dummy.C.get_static0", None, None, n7, None]
          n9 <- $Call(n6, n8, None)
          n10 <- $Call(n5, n9, None)
          TOPLEVEL[get_static0] <- n10
          n11 <- TOPLEVEL[staticmethod]
          n12 <- TOPLEVEL[abstractmethod]
          n13 <- $BuildConstKeyMap($BuildTuple("return"), None, None)
          n14 <- $MakeFunction["get_static1", "dummy.C.get_static1", None, None, n13, None]
          n15 <- $Call(n12, n14, None)
          n16 <- $Call(n11, n15, None)
          TOPLEVEL[get_static1] <- n16
          return None


      function dummy.C.get(self):
        b0:
          return None


      function dummy.C.get_static0():
        b0:
          return None


      function dummy.C.get_static1():
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

      function toplevel():
        b0:
          n0 <- $MakeFunction["C", "dummy.C", None, None, None, None]
          n1 <- $BuildClass(n0, "C", None)
          TOPLEVEL[C] <- n1
          return None


      function dummy.C():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          n1 <- TOPLEVEL[foo]
          n2 <- TOPLEVEL[x]
          n3 <- TOPLEVEL[y]
          n4 <- TOPLEVEL[z]
          n5 <- $Call(n1, n2, n3, n4, None)
          n6 <- $MakeFunction["f", "dummy.C.f", None, None, None, None]
          n7 <- $Call(n5, n6, None)
          TOPLEVEL[f] <- n7
          n8 <- TOPLEVEL[foo]
          n9 <- TOPLEVEL[x]
          n10 <- TOPLEVEL[y]
          n11 <- TOPLEVEL[z]
          n12 <- $CallMethod[bar](n8, n9, n10, n11, None)
          n13 <- $MakeFunction["g", "dummy.C.g", None, None, None, None]
          n14 <- $Call(n12, n13, None)
          TOPLEVEL[g] <- n14
          return None


      function dummy.C.f(self):
        b0:
          return None


      function dummy.C.g(self):
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

      function toplevel():
        b0:
          n0 <- $ImportName(unittest, None, 0)
          TOPLEVEL[unittest] <- n0
          n1 <- $MakeFunction["PwdTest", "dummy.PwdTest", None, None, None, None]
          n2 <- TOPLEVEL[unittest]
          n3 <- n2.TestCase
          n4 <- $BuildClass(n1, "PwdTest", n3, None)
          TOPLEVEL[PwdTest] <- n4
          return None


      function dummy.PwdTest():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "PwdTest"
          n1 <- $MakeFunction["test_values", "dummy.PwdTest.test_values", None, None, None, None]
          TOPLEVEL[test_values] <- n1
          return None


      function dummy.PwdTest.test_values(self, e):
        b0:
          n0 <- LOCAL[self]
          n1 <- GLOBAL[type]
          n2 <- LOCAL[e]
          n3 <- n2.pw_gecos
          n4 <- $Call(n1, n3, None)
          n5 <- GLOBAL[str]
          n6 <- GLOBAL[type]
          n7 <- $Call(n6, None, None)
          n8 <- $CallMethod[assertIn](n0, n4, $BuildTuple(n5, n7), None)
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

      function toplevel():
        b0:
          n0 <- $MakeFunction["C", "dummy.C", None, None, None, None]
          n1 <- TOPLEVEL[Exception]
          n2 <- $BuildClass(n0, "C", n1, None)
          TOPLEVEL[C] <- n2
          n3 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n3
          n4 <- $MakeFunction["g", "dummy.g", None, None, None, None]
          TOPLEVEL[g] <- n4
          return None


      function dummy.C():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          return None


      function dummy.f():
        b0:
          n0 <- GLOBAL[C]
          throw n0


      function dummy.g():
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

      function toplevel():
        b0:
          n0 <- $MakeFunction["C", "dummy.C", None, None, None, None]
          n1 <- $BuildClass(n0, "C", None)
          TOPLEVEL[C] <- n1
          n2 <- $MakeFunction["f", "dummy.f", $BuildTuple(1, 2, "zuck"), None, None, None]
          TOPLEVEL[f] <- n2
          n3 <- TOPLEVEL[f]
          n4 <- $Call(n3, 0, None)
          n5 <- TOPLEVEL[f]
          n6 <- $Call(n5, 10, 100, None)
          n7 <- TOPLEVEL[f]
          n8 <- $Call(n7, 100, 1000, 0, None)
          n9 <- TOPLEVEL[f]
          n10 <- $Call(n9, 0, 0, 0, "toto", None)
          return None


      function dummy.C():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          return None


      function dummy.f(x, y, z, s):
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

      function toplevel():
        b0:
          n0 <- $MakeFunction["TestHook", "dummy.TestHook", None, None, None, None]
          n1 <- $BuildClass(n0, "TestHook", None)
          TOPLEVEL[TestHook] <- n1
          return None


      function dummy.TestHook():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "TestHook"
          n1 <- TOPLEVEL[RuntimeError]
          n2 <- $MakeFunction["__init__", "dummy.TestHook.__init__", $BuildTuple(None, n1), None, None, None]
          TOPLEVEL[__init__] <- n2
          return None


      function dummy.TestHook.__init__(self, raise_on_events, exc_type):
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

      function toplevel():
        b0:
          n0 <- $MakeFunction["C", "dummy.C", None, None, None, None]
          n1 <- $BuildClass(n0, "C", None)
          TOPLEVEL[C] <- n1
          n2 <- TOPLEVEL[C]
          n3 <- $Call(n2, None)
          TOPLEVEL[c] <- n3
          n4 <- TOPLEVEL[c]
          n5 <- $CallMethod[f](n4, 0, None)
          n6 <- TOPLEVEL[c]
          n7 <- $CallMethod[f](n6, 0, 1, None)
          n8 <- TOPLEVEL[c]
          n9 <- $CallMethod[f](n8, 0, 1, 2, None)
          return None


      function dummy.C():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          n1 <- $MakeFunction["f", "dummy.C.f", $BuildTuple(1, 10), None, None, None]
          TOPLEVEL[f] <- n1
          return None


      function dummy.C.f(self, x, y, z):
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

      function toplevel():
        b0:
          n0 <- $MakeFunction["C", "dummy.C", None, None, None, None]
          n1 <- $BuildClass(n0, "C", None)
          TOPLEVEL[C] <- n1
          return None


      function dummy.C():
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

      function toplevel():
        b0:
          n0 <- $MakeFunction["defaultdict", "dummy.defaultdict", None, None, None, None]
          n1 <- $BuildClass(n0, "defaultdict", None)
          TOPLEVEL[defaultdict] <- n1
          return None


      function dummy.defaultdict.__getitem__(self, key):
        b0:
          return 42


      function dummy.defaultdict():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "defaultdict"
          n1 <- $MakeFunction["__getitem__", "dummy.defaultdict.__getitem__", None, None, None, None]
          TOPLEVEL[__getitem__] <- n1
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

      function toplevel():
        b0:
          n0 <- $ImportName(itertools, None, 0)
          TOPLEVEL[itertools] <- n0
          n1 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n1
          n2 <- $MakeFunction["AsyncYieldFrom", "dummy.AsyncYieldFrom", None, None, None, None]
          n3 <- $BuildClass(n2, "AsyncYieldFrom", None)
          TOPLEVEL[AsyncYieldFrom] <- n3
          n4 <- $MakeFunction["powerset", "dummy.powerset", None, None, None, None]
          TOPLEVEL[powerset] <- n4
          return None


      function dummy.AsyncYieldFrom():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "AsyncYieldFrom"
          n1 <- $MakeFunction["__await__", "dummy.AsyncYieldFrom.__await__", None, None, None, None]
          TOPLEVEL[__await__] <- n1
          return None


      function dummy.AsyncYieldFrom.__await__(self):
        b0:
          n0 <- LOCAL[self]
          n1 <- n0.obj
          n2 <- $GetYieldFromIter(n1, None)
          n3 <- $YieldFrom(n2, None, None)
          return None


      function dummy.f():
        b0:
          n0 <- $Yield(42)
          return None


      function dummy.powerset(s):
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

      function toplevel():
        b0:
          n0 <- $MakeFunction["C", "dummy.C", None, None, None, None]
          n1 <- $BuildClass(n0, "C", None)
          TOPLEVEL[C] <- n1
          n2 <- TOPLEVEL[C]
          n3 <- $Call(n2, None)
          TOPLEVEL[c] <- n3
          $Delete(TOPLEVEL[c])
          n4 <- TOPLEVEL[C]
          n5 <- $Call(n4, None)
          GLOBAL[c0] <- n5
          n6 <- GLOBAL[c0]
          $DeleteAttr(n6, foo)
          n7 <- $MakeFunction["f", "dummy.f", None, None, None, None]
          TOPLEVEL[f] <- n7
          n8 <- $MakeFunction["g", "dummy.g", None, None, None, None]
          TOPLEVEL[g] <- n8
          return None


      function dummy.C():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          return None


      function dummy.f(x):
        b0:
          $Delete(GLOBAL[c0])
          $Delete(LOCAL[x])
          $StoreDeref(0,"z", 0)
          n0 <- $LoadClosure(0,"z")
          n1 <- $MakeFunction["inner", "dummy.f.inner", None, None, None, $BuildTuple(n0)]
          LOCAL[inner] <- n1
          return None


      function dummy.g(a, b):
        b0:
          n0 <- LOCAL[a]
          n1 <- LOCAL[b]
          n2 <- $DeleteSubscr(n0, n1, None)
          return None


      function dummy.f.inner():
        b0:
          n0 <- GLOBAL[print]
          n1 <- $LoadDeref(0,"z")
          n2 <- $Call(n0, n1, None)
          $DeleteDeref[0,"z")
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

      function toplevel():
        b0:
          n0 <- $MakeFunction["C", "dummy.C", None, None, None, None]
          n1 <- $BuildClass(n0, "C", None)
          TOPLEVEL[C] <- n1
          return None


      function dummy.C():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          n1 <- $MakeFunction["f", "dummy.C.f", None, None, None, None]
          TOPLEVEL[f] <- n1
          return None


      function dummy.C.f.D():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C.f.<locals>.D"
          n1 <- $LoadClassDeref(0,"binascii")
          n2 <- n1.unhexlify
          n3 <- $MakeFunction["g", "dummy.C.f.D.g", $BuildTuple(n2), None, None, None]
          TOPLEVEL[g] <- n3
          return None


      function dummy.C.f(self):
        b0:
          n0 <- $ImportName(binascii, None, 0)
          $StoreDeref(0,"binascii", n0)
          n1 <- $LoadClosure(0,"binascii")
          n2 <- $MakeFunction["D", "dummy.C.f.D", None, None, None, $BuildTuple(n1)]
          n3 <- $BuildClass(n2, "D", None)
          LOCAL[D] <- n3
          return None


      function dummy.C.f.D.g(self, unhexlify):
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

      function toplevel():
        b0:
          n0 <- $MakeFunction["C", "dummy.C", None, None, None, None]
          n1 <- TOPLEVEL[m]
          n2 <- $BuildClass(n0, "C", n1, $BuildTuple("metaclass"))
          TOPLEVEL[C] <- n2
          return None


      function dummy.C():
        b0:
          n0 <- TOPLEVEL[__name__]
          TOPLEVEL[__module__] <- n0
          TOPLEVEL[__qualname__] <- "C"
          return None |}]
