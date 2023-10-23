(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

let dummy = "dummy.py"

let test ~filename ~debug source =
  if not (Py.is_initialized ()) then Py.initialize ~interpreter:Version.python_exe () ;
  let code =
    match FFI.from_string ~source ~filename with
    | Error (kind, err) ->
        L.die kind "FFI error: %a@\n" FFI.Error.pp_kind err
    | Ok code ->
        code
  in
  Py.finalize () ;
  (* Since Textual doesn't have a concept of toplevel code, we create a function for this code,
     with a non-denotable name, so we don't clash with existing python code *)
  match PyIR.mk ~debug code with
  | Error (kind, _loc, err) -> (
    match kind with
    | L.InternalError ->
        L.internal_error "IR error: %a@\n" PyIR.Error.pp_kind err
    | L.UserError ->
        L.user_error "IR error: %a@\n" PyIR.Error.pp_kind err
    | L.ExternalError ->
        L.external_error "IR error: %a@\n" PyIR.Error.pp_kind err )
  | Ok module_ ->
      F.printf "%a" PyIR.Module.pp module_


let test ?(filename = dummy) ?(debug = false) source =
  try ignore (test ~filename ~debug source)
  with Py.E _ as e -> L.die ExternalError "Pyml exception: %s@\n" (Exn.to_string e)


let%test_module "IR" =
  ( module struct
    let%expect_test _ =
      let source = "x = 42" in
      test source ;
      [%expect {|
module
object dummy:
  code:
    #b0:
      dummy.x <- 42
      return None |}]


    let%expect_test _ =
      let source = {|
x = 42
print(x)
      |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.x <- 42
      n0 <- print(dummy.x)
      return None |}]


    let%expect_test _ =
      let source = {|
x = 42
y = 10
print(x + y)
      |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.x <- 42
      dummy.y <- 10
      n0 <- $Binary.Add(dummy.x, dummy.y)
      n1 <- print(n0)
      return None |}]


    let%expect_test _ =
      let source = {|
x = 42
y = 10
print(x - y)
      |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.x <- 42
      dummy.y <- 10
      n0 <- $Binary.Subtract(dummy.x, dummy.y)
      n1 <- print(n0)
      return None |}]


    let%expect_test _ =
      let source = {|
x = 42
x += 10
print(x)
      |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.x <- 42
      n0 <- $Inplace.Add(dummy.x, 10)
      dummy.x <- n0
      n1 <- print(dummy.x)
      return None |}]


    let%expect_test _ =
      let source = {|
x = 42
x -= 10
print(x)
      |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.x <- 42
      n0 <- $Inplace.Subtract(dummy.x, 10)
      dummy.x <- n0
      n1 <- print(dummy.x)
      return None |}]


    let%expect_test _ =
      let source = {|
pi = 3.14
      |} in
      test source ;
      [%expect {|
module
object dummy:
  code:
    #b0:
      dummy.pi <- 3.14
      return None |}]


    let%expect_test _ =
      let source = {|
byte_data = b'\x48\x65\x6C\x6C\x6F'  # Equivalent to b'Hello'
      |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.byte_data <- "Hello"
      return None |}]


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
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.my_fun <- $FuncObj(my_fun, dummy.my_fun, {})
      dummy.a <- 10
      n0 <- dummy.my_fun(42, dummy.a)
      dummy.z <- n0
      n1 <- print(dummy.z)
      return None



  objects:
    object dummy.my_fun:
      code:
        #b0:
          n0 <- print(x)
          n1 <- print(y)
          n2 <- $Binary.Add(x, y)
          z <- n2
          return z





    functions:
      my_fun -> dummy.my_fun |}]


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
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.update_global <- $FuncObj(update_global, dummy.update_global, {})
      dummy.z <- 0
      n0 <- dummy.update_global()
      n1 <- print(dummy.z)
      return None



  objects:
    object dummy.update_global:
      code:
        #b0:
          n0 <- $Binary.Add(dummy.z, 1)
          dummy.z <- n0
          return None





    functions:
      update_global -> dummy.update_global |}]


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
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.coin <- $FuncObj(coin, dummy.coin, {})
      dummy.f <- $FuncObj(f, dummy.f, {})
      return None



  objects:
    object dummy.coin:
      code:
        #b0:
          return false




    object dummy.f:
      code:
        #b0:
          n0 <- dummy.coin()
          if n0 then jmp b1 else jmp b2


        #b1:
          return x


        #b2:
          return y





    functions:
      coin -> dummy.coin
      f -> dummy.f |}]


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
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.coin <- $FuncObj(coin, dummy.coin, {})
      dummy.f <- $FuncObj(f, dummy.f, {})
      return None



  objects:
    object dummy.coin:
      code:
        #b0:
          return false




    object dummy.f:
      code:
        #b0:
          z <- 0
          n0 <- dummy.coin()
          if n0 then jmp b1 else jmp b2


        #b1:
          z <- x
          jmp b3


        #b2:
          z <- y
          jmp b3


        #b3:
          return z





    functions:
      coin -> dummy.coin
      f -> dummy.f |}]


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
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.coin <- $FuncObj(coin, dummy.coin, {})
      dummy.f <- $FuncObj(f, dummy.f, {})
      return None



  objects:
    object dummy.coin:
      code:
        #b0:
          return false




    object dummy.f:
      code:
        #b0:
          z <- 0
          n0 <- dummy.coin()
          if n0 then jmp b1 else jmp b2


        #b1:
          n1 <- dummy.coin()
          if n1 then jmp b3 else jmp b4


        #b3:
          z <- x
          jmp b5


        #b4:
          return 1664


        #b5:
          n2 <- $Binary.Add(z, 1)
          z <- n2
          jmp b6


        #b2:
          n3 <- $Binary.Add(z, 1)
          z <- n3
          n4 <- dummy.coin()
          if n4 then jmp b7 else jmp b8


        #b7:
          return 42


        #b8:
          z <- y
          jmp b6


        #b6:
          return z





    functions:
      coin -> dummy.coin
      f -> dummy.f |}]


    let%expect_test _ =
      let source = {|
def foo(x):
    pass

def f(x):
    foo(1 if x else 0)
      |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.foo <- $FuncObj(foo, dummy.foo, {})
      dummy.f <- $FuncObj(f, dummy.f, {})
      return None



  objects:
    object dummy.foo:
      code:
        #b0:
          return None




    object dummy.f:
      code:
        #b0:
          if x then jmp b1(dummy.foo) else jmp b2(dummy.foo)


        #b1(n0):
          jmp b3(1, n0)


        #b2(n1):
          jmp b3(0, n1)


        #b3(n3, n2):
          n4 <- n2(n3)
          return None





    functions:
      f -> dummy.f
      foo -> dummy.foo |}]


    let%expect_test _ =
      let source = {|
for x in range(10):
    print(x)
      |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      n0 <- range(10)
      n1 <- $GetIter(n0)
      jmp b1(n1)


    #b1(n2):
      n3 <- $NextIter(n2)
      n4 <- $HasNextIter(n3)
      if n4 then jmp b2 else jmp b3


    #b2:
      n5 <- $IterData(n3)
      dummy.x <- n5
      n6 <- print(dummy.x)
      jmp b1(n2)


    #b3:
      return None |}]


    let%expect_test _ =
      let source = {|
l = [0, 1, 2, 3, 4, 5]
l[0:2]
l[0:2:1]
          |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.l <- [0, 1, 2, 3, 4, 5]
      n0 <- dummy.l[[0:2]]
      n1 <- dummy.l[[0:2:1]]
      return None |}]


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
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      n0 <- print(42)
      dummy.print <- $FuncObj(print, dummy.print, {})
      n1 <- dummy.print(42)
      dummy.f <- $FuncObj(f, dummy.f, {})
      return None



  objects:
    object dummy.print:
      code:
        #b0:
          return x




    object dummy.f:
      code:
        #b0:
          n0 <- dummy.print(x)
          return None





    functions:
      f -> dummy.f
      print -> dummy.print |}]


    let%expect_test _ =
      let source =
        {|
def f0(x: int, y, z:float):
        pass

def f1(x, y:str) -> bool:
        pass
        |}
      in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.f0 <- $FuncObj(f0, dummy.f0, {})
      dummy.f1 <- $FuncObj(f1, dummy.f1, {})
      return None



  objects:
    object dummy.f0:
      code:
        #b0:
          return None




    object dummy.f1:
      code:
        #b0:
          return None





    functions:
      f0 -> dummy.f0
      f1 -> dummy.f1 |}]


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
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.expect_int <- $FuncObj(expect_int, dummy.expect_int, {})
      dummy.get <- $FuncObj(get, dummy.get, {})
      n0 <- dummy.get()
      n1 <- dummy.expect_int(n0)
      return None



  objects:
    object dummy.expect_int:
      code:
        #b0:
          return None




    object dummy.get:
      code:
        #b0:
          return 42





    functions:
      expect_int -> dummy.expect_int
      get -> dummy.get |}]


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
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.expect <- $FuncObj(expect, dummy.expect, {})
      dummy.get <- $FuncObj(get, dummy.get, {})
      n0 <- dummy.get()
      n1 <- dummy.expect(n0)
      return None



  objects:
    object dummy.expect:
      code:
        #b0:
          return None




    object dummy.get:
      code:
        #b0:
          return 42





    functions:
      expect -> dummy.expect
      get -> dummy.get |}]


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
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.C <- $ClassObj($FuncObj(C, dummy.C, {}), "C")
      n0 <- dummy.C(0, "a")
      dummy.c <- n0
      n1 <- dummy.c.x
      n2 <- $CallMethod($LoadMethod(dummy.c, get), )
      n3 <- $CallMethod($LoadMethod(dummy.c, set), 42)
      return None



  objects:
    object dummy.C:
      code:
        #b0:
          dummy.C.__module__ <- __name__
          dummy.C.__qualname__ <- "C"
          dummy.C.__init__ <- $FuncObj(__init__, dummy.C.__init__, {})
          dummy.C.get <- $FuncObj(get, dummy.C.get, {})
          dummy.C.set <- $FuncObj(set, dummy.C.set, {})
          return None



      objects:
        object dummy.C.__init__:
          code:
            #b0:
              self.x <- x
              self.y <- y
              return None




        object dummy.C.get:
          code:
            #b0:
              return self.x




        object dummy.C.set:
          code:
            #b0:
              self.x <- x
              return None





        functions:
          __init__ -> dummy.C.__init__
          get -> dummy.C.get
          set -> dummy.C.set


        classes:
          C

        functions:
          C -> dummy.C |}]


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

        # Stupid function to test the staticmethod decorator + type annotations
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
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.IntBox <- $ClassObj($FuncObj(IntBox, dummy.IntBox, {}), "IntBox")
      dummy.getX <- $FuncObj(getX, dummy.getX, {})
      n0 <- dummy.IntBox(10)
      dummy.c <- n0
      n1 <- dummy.c.x
      dummy.c.z <- 10
      n2 <- $CallMethod($LoadMethod(dummy.c, get), )
      n3 <- $CallMethod($LoadMethod(dummy.c, set), 42)
      n4 <- $CallMethod($LoadMethod(dummy.c, run), )
      n5 <- print(dummy.c.z)
      return None



  objects:
    object dummy.IntBox:
      code:
        #b0:
          dummy.IntBox.__module__ <- __name__
          dummy.IntBox.__qualname__ <- "IntBox"
          $SETUP_ANNOTATIONS
          dummy.IntBox.__annotations__["x"] <- int
          dummy.IntBox.__init__ <- $FuncObj(__init__, dummy.IntBox.__init__, {})
          dummy.IntBox.get <- $FuncObj(get, dummy.IntBox.get, {})
          dummy.IntBox.set <- $FuncObj(set, dummy.IntBox.set, {})
          dummy.IntBox.run <- $FuncObj(run, dummy.IntBox.run, {})
          n0 <- staticmethod($FuncObj(id, dummy.IntBox.id, {}))
          dummy.IntBox.id <- n0
          return None



      objects:
        object dummy.IntBox.__init__:
          code:
            #b0:
              self.x <- x
              return None




        object dummy.IntBox.get:
          code:
            #b0:
              return self.x




        object dummy.IntBox.set:
          code:
            #b0:
              self.x <- x
              return None




        object dummy.IntBox.run:
          code:
            #b0:
              return None




        object dummy.IntBox.id:
          code:
            #b0:
              return x





        functions:
          __init__ -> dummy.IntBox.__init__
          get -> dummy.IntBox.get
          id -> dummy.IntBox.id
          run -> dummy.IntBox.run
          set -> dummy.IntBox.set

        object dummy.getX:
          code:
            #b0:
              n0 <- $CallMethod($LoadMethod(box, get), )
              return n0





        classes:
          IntBox

        functions:
          IntBox -> dummy.IntBox
          getX -> dummy.getX |}]


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
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.C <- $ClassObj($FuncObj(C, dummy.C, {}), "C")
      dummy.D <- $ClassObj($FuncObj(D, dummy.D, {}), "D", dummy.C)
      return None



  objects:
    object dummy.C:
      code:
        #b0:
          dummy.C.__module__ <- __name__
          dummy.C.__qualname__ <- "C"
          n0 <- staticmethod($FuncObj(f, dummy.C.f, {}))
          dummy.C.f <- n0
          n1 <- staticmethod($FuncObj(typed_f, dummy.C.typed_f, {}))
          dummy.C.typed_f <- n1
          return None



      objects:
        object dummy.C.f:
          code:
            #b0:
              return None




        object dummy.C.typed_f:
          code:
            #b0:
              return x





        functions:
          f -> dummy.C.f
          typed_f -> dummy.C.typed_f

        object dummy.D:
          code:
            #b0:
              dummy.D.__module__ <- __name__
              dummy.D.__qualname__ <- "D"
              return None





        classes:
          C
          D

        functions:
          C -> dummy.C
          D -> dummy.D |}]


    let%expect_test _ =
      let source = {|
class C:
    @staticmethod
    def f():
          pass

C.f()
        |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.C <- $ClassObj($FuncObj(C, dummy.C, {}), "C")
      n0 <- $CallMethod($LoadMethod(dummy.C, f), )
      return None



  objects:
    object dummy.C:
      code:
        #b0:
          dummy.C.__module__ <- __name__
          dummy.C.__qualname__ <- "C"
          n0 <- staticmethod($FuncObj(f, dummy.C.f, {}))
          dummy.C.f <- n0
          return None



      objects:
        object dummy.C.f:
          code:
            #b0:
              return None





        functions:
          f -> dummy.C.f


    classes:
      C

    functions:
      C -> dummy.C |}]


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
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.A <- $ClassObj($FuncObj(A, dummy.A, {}), "A")
      dummy.C <- $ClassObj($FuncObj(C, dummy.C, {}), "C")
      dummy.g <- $FuncObj(g, dummy.g, {})
      return None



  objects:
    object dummy.A:
      code:
        #b0:
          dummy.A.__module__ <- __name__
          dummy.A.__qualname__ <- "A"
          dummy.A.f <- $FuncObj(f, dummy.A.f, {})
          return None



      objects:
        object dummy.A.f:
          code:
            #b0:
              return None





        functions:
          f -> dummy.A.f

      object dummy.C:
        code:
          #b0:
            dummy.C.__module__ <- __name__
            dummy.C.__qualname__ <- "C"
            $SETUP_ANNOTATIONS
            dummy.C.__annotations__["a"] <- dummy.A
            return None




      object dummy.g:
        code:
          #b0:
            n0 <- $CallMethod($LoadMethod(c.a, f), )
            n1 <- print(n0)
            return None





      classes:
        A
        C

      functions:
        A -> dummy.A
        C -> dummy.C
        g -> dummy.g |}]


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
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.A <- $ClassObj($FuncObj(A, dummy.A, {}), "A")
      dummy.B <- $ClassObj($FuncObj(B, dummy.B, {}), "B")
      dummy.C <- $ClassObj($FuncObj(C, dummy.C, {}), "C", dummy.A, dummy.B)
      return None



  objects:
    object dummy.A:
      code:
        #b0:
          dummy.A.__module__ <- __name__
          dummy.A.__qualname__ <- "A"
          return None




    object dummy.B:
      code:
        #b0:
          dummy.B.__module__ <- __name__
          dummy.B.__qualname__ <- "B"
          return None




    object dummy.C:
      code:
        #b0:
          dummy.C.__module__ <- __name__
          dummy.C.__qualname__ <- "C"
          return None





    classes:
      A
      B
      C

    functions:
      A -> dummy.A
      B -> dummy.B
      C -> dummy.C |}]


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
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.C <- $ClassObj($FuncObj(C, dummy.C, {}), "C")
      dummy.build <- $FuncObj(build, dummy.build, {})
      n0 <- dummy.build()
      dummy.cs <- n0
      n1 <- dummy.cs[0].x
      return None



  objects:
    object dummy.C:
      code:
        #b0:
          dummy.C.__module__ <- __name__
          dummy.C.__qualname__ <- "C"
          dummy.C.__init__ <- $FuncObj(__init__, dummy.C.__init__, {})
          return None



      objects:
        object dummy.C.__init__:
          code:
            #b0:
              self.x <- 0
              return None





        functions:
          __init__ -> dummy.C.__init__

      object dummy.build:
        code:
          #b0:
            n0 <- dummy.C()
            return [n0]





      classes:
        C

      functions:
        C -> dummy.C
        build -> dummy.build |}]


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
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.f <- $FuncObj(f, dummy.f, {})
      n0 <- dummy.f()
      return None



  objects:
    object dummy.f:
      code:
        #b0:
          A <- $ClassObj($FuncObj(A, dummy.A, {}), "A")
          n0 <- A()
          a <- n0
          n1 <- $CallMethod($LoadMethod(a, get), )
          return n1



      objects:
        object dummy.f.A:
          code:
            #b0:
              dummy.f.A.__module__ <- __name__
              dummy.f.A.__qualname__ <- "f.<locals>.A"
              dummy.f.A.__init__ <- $FuncObj(__init__, dummy.f.<locals>.A.__init__, {})
              dummy.f.A.get <- $FuncObj(get, dummy.f.<locals>.A.get, {})
              return None



          objects:
            object dummy.f.A.__init__:
              code:
                #b0:
                  self.x <- 0
                  return None




            object dummy.f.A.get:
              code:
                #b0:
                  return self.x





            functions:
              __init__ -> dummy.f.<locals>.A.__init__
              get -> dummy.f.<locals>.A.get


          classes:
            A

          functions:
            A -> dummy.A


        functions:
          f -> dummy.f |}]


    let%expect_test _ =
      let source =
        {|
import base
import base # should only call base.$toplevel once

base.f(0)
        |}
      in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      $ImportName(base, from_list= [])
      dummy.base <- $ImportName(base, from_list= [])
      $ImportName(base, from_list= [])
      dummy.base <- $ImportName(base, from_list= [])
      n0 <- $CallMethod($LoadMethod(base, f), 0)
      return None |}]


    let%expect_test _ =
      let source =
        {|

def f():
        pass

f()

from base import f, g

f()
from base import f, g # to test that import.toplevel is only called once
g()
        |}
      in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.f <- $FuncObj(f, dummy.f, {})
      n0 <- dummy.f()
      $ImportName(base, from_list= [f, g])
      dummy.f <- $ImportFrom($ImportName(base, from_list= [f, g]), name= f)
      dummy.g <- $ImportFrom($ImportName(base, from_list= [f, g]), name= g)
      n1 <- base.f()
      $ImportName(base, from_list= [f, g])
      dummy.f <- $ImportFrom($ImportName(base, from_list= [f, g]), name= f)
      dummy.g <- $ImportFrom($ImportName(base, from_list= [f, g]), name= g)
      n2 <- base.g()
      return None



  objects:
    object dummy.f:
      code:
        #b0:
          return None





    functions:
      f -> dummy.f |}]


    let%expect_test _ =
      let source = {|
import unittest

class MyTest(unittest.TestCase):
        pass
        |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      $ImportName(unittest, from_list= [])
      dummy.unittest <- $ImportName(unittest, from_list= [])
      dummy.MyTest <- $ClassObj($FuncObj(MyTest, dummy.MyTest, {}), "MyTest", unittest.TestCase)
      return None



  objects:
    object dummy.MyTest:
      code:
        #b0:
          dummy.MyTest.__module__ <- __name__
          dummy.MyTest.__qualname__ <- "MyTest"
          return None





    classes:
      MyTest

    functions:
      MyTest -> dummy.MyTest |}]


    (* Extracted from Cinder's test suite. Currently amended to avoid unsupported opcodes *)
    let%expect_test _ =
      let source =
        {|
import os
import sys
from test.libregrtest import main


main_in_temp_cwd = main


def _main():
    global __file__

    mydir = os.path.abspath(os.path.normpath(os.path.dirname(sys.argv[0])))
    i = len(sys.path) - 1
    while i >= 0:
        if os.path.abspath(os.path.normpath(sys.path[i])) == mydir:
            # del sys.path[i] # not supported yet
            pass
        else:
            i -= 1

    __file__ = os.path.abspath(__file__)

    # sanity check
    # assert __file__ == os.path.abspath(sys.argv[0]) # not supported yet

    main()


if __name__ == '__main__':
    _main()
      |}
      in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      $ImportName(os, from_list= [])
      dummy.os <- $ImportName(os, from_list= [])
      $ImportName(sys, from_list= [])
      dummy.sys <- $ImportName(sys, from_list= [])
      $ImportName(test.libregrtest, from_list= [main])
      dummy.main <- $ImportFrom($ImportName(test.libregrtest, from_list= [main]), name= main)
      dummy.main_in_temp_cwd <- test.libregrtest.main
      dummy._main <- $FuncObj(_main, dummy._main, {})
      n0 <- $Compare.eq(__name__, "__main__")
      if n0 then jmp b1 else jmp b2


    #b1:
      n1 <- dummy._main()
      jmp b2


    #b2:
      return None



  objects:
    object dummy._main:
      code:
        #b0:
          n0 <- $CallMethod($LoadMethod(os.path, dirname), sys.argv[0])
          n1 <- $CallMethod($LoadMethod(os.path, normpath), n0)
          n2 <- $CallMethod($LoadMethod(os.path, abspath), n1)
          mydir <- n2
          n3 <- len(sys.path)
          n4 <- $Binary.Subtract(n3, 1)
          i <- n4
          jmp b1


        #b1:
          n5 <- $Compare.ge(i, 0)
          if n5 then jmp b2 else jmp b3


        #b2:
          n6 <- $CallMethod($LoadMethod(os.path, normpath), sys.path[i])
          n7 <- $CallMethod($LoadMethod(os.path, abspath), n6)
          n8 <- $Compare.eq(n7, mydir)
          if n8 then jmp b4 else jmp b5


        #b4:
          jmp b1


        #b5:
          n9 <- $Inplace.Subtract(i, 1)
          i <- n9
          jmp b1


        #b3:
          n10 <- $CallMethod($LoadMethod(os.path, abspath), __file__)
          dummy.__file__ <- n10
          n11 <- test.libregrtest.main()
          return None





    functions:
      _main -> dummy._main |}]


    let%expect_test _ =
      let source =
        {|
from A import X
X()
from .B import X
X()
from ..C import X
X()

from .. import path
# this will generate a warning, expected until modules are encoded as proper Textual types
path.X()
      |}
      in
      test ~filename:"some/long/path/dummy.py" source ;
      [%expect
        {|
module
object some.long.path.dummy:
  code:
    #b0:
      $ImportName(A, from_list= [X])
      some.long.path.dummy.X <- $ImportFrom($ImportName(A, from_list= [X]), name= X)
      n0 <- A.X()
      $ImportName(some.long.path.B, from_list= [X])
      some.long.path.dummy.X <- $ImportFrom($ImportName(some.long.path.B, from_list= [X]), name= X)
      n1 <- some.long.path.B.X()
      $ImportName(some.long.C, from_list= [X])
      some.long.path.dummy.X <- $ImportFrom($ImportName(some.long.C, from_list= [X]), name= X)
      n2 <- some.long.C.X()
      $ImportName(some.long, from_list= [path])
      some.long.path.dummy.path <- $ImportFrom($ImportName(some.long, from_list= [path]),
        name= path)
      n3 <- $CallMethod($LoadMethod(some.long.path, X), )
      return None |}]


    let%expect_test _ =
      let source =
        {|
from x import y as z, a as b
from x import y as z, a as b #testing the single load of x's top level

z()
b()

from foo import toto, tata #testing the single load of foo's top level
from foo import toto, tata
toto()
tata()

        |}
      in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      $ImportName(x, from_list= [y, a])
      dummy.z <- $ImportFrom($ImportName(x, from_list= [y, a]), name= y)
      dummy.b <- $ImportFrom($ImportName(x, from_list= [y, a]), name= a)
      $ImportName(x, from_list= [y, a])
      dummy.z <- $ImportFrom($ImportName(x, from_list= [y, a]), name= y)
      dummy.b <- $ImportFrom($ImportName(x, from_list= [y, a]), name= a)
      n0 <- x.y()
      n1 <- x.a()
      $ImportName(foo, from_list= [toto, tata])
      dummy.toto <- $ImportFrom($ImportName(foo, from_list= [toto, tata]), name= toto)
      dummy.tata <- $ImportFrom($ImportName(foo, from_list= [toto, tata]), name= tata)
      $ImportName(foo, from_list= [toto, tata])
      dummy.toto <- $ImportFrom($ImportName(foo, from_list= [toto, tata]), name= toto)
      dummy.tata <- $ImportFrom($ImportName(foo, from_list= [toto, tata]), name= tata)
      n2 <- foo.toto()
      n3 <- foo.tata()
      return None |}]


    let%expect_test _ =
      let source = {|
# From Cinder
import xml.etree.ElementTree as ET
          |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      $ImportName(xml.etree.ElementTree, from_list= [])
      dummy.ET <- $ImportFrom($ImportName(xml.etree.ElementTree, from_list= []),
        name= etree.ElementTree)
      return None
          |}]


    let%expect_test _ =
      let source = {|
class C:
  pass

class D(C):
  pass
  |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.C <- $ClassObj($FuncObj(C, dummy.C, {}), "C")
      dummy.D <- $ClassObj($FuncObj(D, dummy.D, {}), "D", dummy.C)
      return None



  objects:
    object dummy.C:
      code:
        #b0:
          dummy.C.__module__ <- __name__
          dummy.C.__qualname__ <- "C"
          return None




    object dummy.D:
      code:
        #b0:
          dummy.D.__module__ <- __name__
          dummy.D.__qualname__ <- "D"
          return None





    classes:
      C
      D

    functions:
      C -> dummy.C
      D -> dummy.D |}]


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
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.C <- $ClassObj($FuncObj(C, dummy.C, {}), "C")
      dummy.D <- $ClassObj($FuncObj(D, dummy.D, {}), "D", dummy.C)
      dummy.C0 <- $ClassObj($FuncObj(C0, dummy.C0, {}), "C0")
      dummy.D0 <- $ClassObj($FuncObj(D0, dummy.D0, {}), "D0", dummy.C0)
      return None



  objects:
    object dummy.C:
      code:
        #b0:
          dummy.C.__module__ <- __name__
          dummy.C.__qualname__ <- "C"
          return None




    object dummy.D:
      code:
        #b0:
          dummy.D.__module__ <- __name__
          dummy.D.__qualname__ <- "D"
          dummy.D.__init__ <- $FuncObj(__init__, dummy.D.__init__, {})
          dummy.D.__classcell__ <- $LoadClosure(__class__)
          return $LoadClosure(__class__)



      objects:
        object dummy.D.__init__:
          code:
            #b0:
              n0 <- super()
              n1 <- $CallMethod($LoadMethod(n0, __init__), )
              return None





        functions:
          __init__ -> dummy.D.__init__

      object dummy.C0:
        code:
          #b0:
            dummy.C0.__module__ <- __name__
            dummy.C0.__qualname__ <- "C0"
            dummy.C0.__init__ <- $FuncObj(__init__, dummy.C0.__init__, {})
            return None



        objects:
          object dummy.C0.__init__:
            code:
              #b0:
                foo.x <- x
                return None





          functions:
            __init__ -> dummy.C0.__init__

        object dummy.D0:
          code:
            #b0:
              dummy.D0.__module__ <- __name__
              dummy.D0.__qualname__ <- "D0"
              dummy.D0.__init__ <- $FuncObj(__init__, dummy.D0.__init__, {})
              dummy.D0.__classcell__ <- $LoadClosure(__class__)
              return $LoadClosure(__class__)



          objects:
            object dummy.D0.__init__:
              code:
                #b0:
                  n0 <- super()
                  n1 <- $CallMethod($LoadMethod(n0, __init__), 42)
                  return None





            functions:
              __init__ -> dummy.D0.__init__


        classes:
          C
          C0
          D
          D0

        functions:
          C -> dummy.C
          C0 -> dummy.C0
          D -> dummy.D
          D0 -> dummy.D0 |}]


    let%expect_test _ =
      let source =
        {|
import foo

class C(foo.D):
        def __init__(self, x):
          super().__init__(x)
        |}
      in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      $ImportName(foo, from_list= [])
      dummy.foo <- $ImportName(foo, from_list= [])
      dummy.C <- $ClassObj($FuncObj(C, dummy.C, {}), "C", foo.D)
      return None



  objects:
    object dummy.C:
      code:
        #b0:
          dummy.C.__module__ <- __name__
          dummy.C.__qualname__ <- "C"
          dummy.C.__init__ <- $FuncObj(__init__, dummy.C.__init__, {})
          dummy.C.__classcell__ <- $LoadClosure(__class__)
          return $LoadClosure(__class__)



      objects:
        object dummy.C.__init__:
          code:
            #b0:
              n0 <- super()
              n1 <- $CallMethod($LoadMethod(n0, __init__), x)
              return None





        functions:
          __init__ -> dummy.C.__init__


    classes:
      C

    functions:
      C -> dummy.C |}]


    let%expect_test _ =
      let source = {|
def f(x, y):
  return (x == y)
        |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.f <- $FuncObj(f, dummy.f, {})
      return None



  objects:
    object dummy.f:
      code:
        #b0:
          n0 <- $Compare.eq(x, y)
          return n0





    functions:
      f -> dummy.f |}]


    let%expect_test _ =
      let source = "True != False" in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      n0 <- $Compare.neq(true, false)
      return None |}]


    let%expect_test _ =
      let source = {|
def f(x, y, z, t):
        return (x and y) or (z and t)
        |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.f <- $FuncObj(f, dummy.f, {})
      return None



  objects:
    object dummy.f:
      code:
        #b0:
          if x then jmp b1 else jmp b2


        #b1:
          if $Not(y) then jmp b2 else jmp b3(y)


        #b2:
          if z then jmp b4 else jmp b3(z)


        #b4:
          jmp b3(t)


        #b3(n1):
          return n1





    functions:
      f -> dummy.f |}]


    let%expect_test _ =
      let source = {|
def f(x, y):
  return (x > y)
        |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.f <- $FuncObj(f, dummy.f, {})
      return None



  objects:
    object dummy.f:
      code:
        #b0:
          n0 <- $Compare.gt(x, y)
          return n0





    functions:
      f -> dummy.f |}]


    let%expect_test _ =
      let source = {|
def f(x, y):
  return (x <= y)
        |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.f <- $FuncObj(f, dummy.f, {})
      return None



  objects:
    object dummy.f:
      code:
        #b0:
          n0 <- $Compare.le(x, y)
          return n0





    functions:
      f -> dummy.f |}]


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
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.is_check <- $FuncObj(is_check, dummy.is_check, {})
      dummy.is_not_check <- $FuncObj(is_not_check, dummy.is_not_check, {})
      dummy.in_check <- $FuncObj(in_check, dummy.in_check, {})
      dummy.in_not_check <- $FuncObj(in_not_check, dummy.in_not_check, {})
      return None



  objects:
    object dummy.is_check:
      code:
        #b0:
          n0 <- $Compare.is(x, None)
          return n0




    object dummy.is_not_check:
      code:
        #b0:
          n0 <- $Compare.is_not(x, None)
          return n0




    object dummy.in_check:
      code:
        #b0:
          n0 <- $Compare.in(x, l)
          return n0




    object dummy.in_not_check:
      code:
        #b0:
          n0 <- $Compare.not_in(x, l)
          return n0





    functions:
      in_check -> dummy.in_check
      in_not_check -> dummy.in_not_check
      is_check -> dummy.is_check
      is_not_check -> dummy.is_not_check |}]


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
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      $ImportName(abc, from_list= [ABC, abstractmethod])
      dummy.ABC <- $ImportFrom($ImportName(abc, from_list= [ABC, abstractmethod]), name= ABC)
      dummy.abstractmethod <- $ImportFrom($ImportName(abc, from_list= [ABC, abstractmethod]),
        name= abstractmethod)
      dummy.C <- $ClassObj($FuncObj(C, dummy.C, {}), "C", abc.ABC)
      return None



  objects:
    object dummy.C:
      code:
        #b0:
          dummy.C.__module__ <- __name__
          dummy.C.__qualname__ <- "C"
          n0 <- abc.abstractmethod($FuncObj(get, dummy.C.get, {}))
          dummy.C.get <- n0
          n1 <- staticmethod($FuncObj(get_static0, dummy.C.get_static0, {}))
          n2 <- abc.abstractmethod(n1)
          dummy.C.get_static0 <- n2
          n3 <- abc.abstractmethod($FuncObj(get_static1, dummy.C.get_static1, {}))
          n4 <- staticmethod(n3)
          dummy.C.get_static1 <- n4
          return None



      objects:
        object dummy.C.get:
          code:
            #b0:
              return None




        object dummy.C.get_static0:
          code:
            #b0:
              return None




        object dummy.C.get_static1:
          code:
            #b0:
              return None





        functions:
          get -> dummy.C.get
          get_static0 -> dummy.C.get_static0
          get_static1 -> dummy.C.get_static1


        classes:
          C

        functions:
          C -> dummy.C |}]


    let%expect_test _ =
      let source = {|
l = [1, 2, 3]
print(l[0])
|} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.l <- [1, 2, 3]
      n0 <- print(dummy.l[0])
      return None |}]


    let%expect_test _ =
      let source = {|
l = [1, 2, 3]
x = 0
l[x] = 10
|} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.l <- [1, 2, 3]
      dummy.x <- 0
      dummy.l[dummy.x] <- 10
      return None |}]


    let%expect_test _ =
      let source =
        {|
t = (1, 2, 3) # will be a constant, not a BUILD_TUPLE
def f(x, y, z):
        return (x, y, z) # should be BUILD_TUPLE
|}
      in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.t <- (1, 2, 3)
      dummy.f <- $FuncObj(f, dummy.f, {})
      return None



  objects:
    object dummy.f:
      code:
        #b0:
          return (x, y, z)





    functions:
      f -> dummy.f |}]


    let%expect_test _ =
      let source = {|
s = {1, 2, 3}
|} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.s <- {1, 2, 3}
      return None |}]


    let%expect_test _ =
      let source = {|
l = [1, 2, 3]
print(l)

def build_list():
          return [1, 2, 3]
|} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.l <- [1, 2, 3]
      n0 <- print(dummy.l)
      dummy.build_list <- $FuncObj(build_list, dummy.build_list, {})
      return None



  objects:
    object dummy.build_list:
      code:
        #b0:
          return [1, 2, 3]





    functions:
      build_list -> dummy.build_list |}]


    let%expect_test _ =
      let source =
        {|
x = "1"
s = {x : 1, "2": 2}
print(s)

s = {"a": 42, "b": 1664}
print(s["1"])

# from cinder
d = { 0x78: "abc", # 1-n decoding mapping
      b"abc": 0x0078,# 1-n encoding mapping
      0x01: None,   # decoding mapping to <undefined>
      0x79: "",    # decoding mapping to <remove character>
      }
        |}
      in
      test source ;
      [%expect
        {xxx|
module
object dummy:
  code:
    #b0:
      dummy.x <- "1"
      dummy.s <- {|dummy.x, 1, "2", 2|}
      n0 <- print(dummy.s)
      dummy.s <- {"a": 42, "b": 1664, }
      n1 <- print(dummy.s["1"])
      dummy.d <- {1: None, 120: "abc", 121: "", "abc": 120, }
      return None |xxx}]


    let%expect_test _ =
      let source =
        {|
import unittest
import signal

@unittest.skipUnless(hasattr(signal, "setitimer"), "requires setitimer()")
class Test(unittest.TestCase):
  pass
  |}
      in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      $ImportName(unittest, from_list= [])
      dummy.unittest <- $ImportName(unittest, from_list= [])
      $ImportName(signal, from_list= [])
      dummy.signal <- $ImportName(signal, from_list= [])
      n0 <- hasattr(signal, "setitimer")
      n1 <- $CallMethod($LoadMethod(unittest, skipUnless), n0, "requires setitimer()")
      n2 <- n1($ClassObj($FuncObj(Test, dummy.Test, {}), "Test", unittest.TestCase))
      dummy.Test <- n2
      return None



  objects:
    object dummy.Test:
      code:
        #b0:
          dummy.Test.__module__ <- __name__
          dummy.Test.__qualname__ <- "Test"
          return None





    classes:
      Test

    functions:
      Test -> dummy.Test |}]


    let%expect_test _ =
      let source =
        {|
# this test will generate "$unknown" values since foo, x, y and z are not defined
class C:
    @foo(x, y, z)
    def f(self):
        pass

    @foo.bar(x, y, z)
    def g(self):
        pass
        |}
      in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.C <- $ClassObj($FuncObj(C, dummy.C, {}), "C")
      return None



  objects:
    object dummy.C:
      code:
        #b0:
          dummy.C.__module__ <- __name__
          dummy.C.__qualname__ <- "C"
          n0 <- $unknown.foo($unknown.x, $unknown.y, $unknown.z)
          n1 <- n0($FuncObj(f, dummy.C.f, {}))
          dummy.C.f <- n1
          n2 <- $CallMethod($LoadMethod($unknown.foo, bar), $unknown.x, $unknown.y, $unknown.z)
          n3 <- n2($FuncObj(g, dummy.C.g, {}))
          dummy.C.g <- n3
          return None



      objects:
        object dummy.C.f:
          code:
            #b0:
              return None




        object dummy.C.g:
          code:
            #b0:
              return None





        functions:
          f -> dummy.C.f
          g -> dummy.C.g


      classes:
        C

      functions:
        C -> dummy.C |}]


    let%expect_test _ =
      let source =
        {|
import unittest

class PwdTest(unittest.TestCase):

    def test_values(self, e):
        self.assertIn(type(e.pw_gecos), (str, type(None)))
      |}
      in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      $ImportName(unittest, from_list= [])
      dummy.unittest <- $ImportName(unittest, from_list= [])
      dummy.PwdTest <- $ClassObj($FuncObj(PwdTest, dummy.PwdTest, {}), "PwdTest", unittest.TestCase)
      return None



  objects:
    object dummy.PwdTest:
      code:
        #b0:
          dummy.PwdTest.__module__ <- __name__
          dummy.PwdTest.__qualname__ <- "PwdTest"
          dummy.PwdTest.test_values <- $FuncObj(test_values, dummy.PwdTest.test_values, {})
          return None



      objects:
        object dummy.PwdTest.test_values:
          code:
            #b0:
              n0 <- type(e.pw_gecos)
              n1 <- type(None)
              n2 <- $CallMethod($LoadMethod(self, assertIn), n0, (str, n1))
              return None





        functions:
          test_values -> dummy.PwdTest.test_values


    classes:
      PwdTest

    functions:
      PwdTest -> dummy.PwdTest |}]


    let%expect_test _ =
      let source = {|
fp = open("foo.txt", "wt")
fp.write("yolo")
          |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      n0 <- open("foo.txt", "wt")
      dummy.fp <- n0
      n1 <- $CallMethod($LoadMethod(dummy.fp, write), "yolo")
      return None |}]


    let%expect_test _ =
      let source = {|
with open("foo.txt", "wt") as fp:
    fp.write("yolo")
          |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      n0 <- open("foo.txt", "wt")
      n1 <- $LoadMethod(n0, __enter__)()
      dummy.fp <- n1
      n2 <- $CallMethod($LoadMethod(dummy.fp, write), "yolo")
      n3 <- CM(n0).__exit__(None, None, None)
      return None |}]


    let%expect_test _ =
      let source =
        {|
try:
      print("TRY BLOCK")
finally:
      print("FINALLY BLOCK")
      |}
      in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      n0 <- print("TRY BLOCK")
      n1 <- print("FINALLY BLOCK")
      return None |}]


    let%expect_test _ =
      let source = {|
def f():
        pass

(a, b) = f()
|} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.f <- $FuncObj(f, dummy.f, {})
      n0 <- dummy.f()
      dummy.a <- n0[0]
      dummy.b <- n0[1]
      return None



  objects:
    object dummy.f:
      code:
        #b0:
          return None





    functions:
      f -> dummy.f |}]


    let%expect_test _ =
      let source =
        {|
def f(**kwargs):
        for (k, v) in kwargs.items():
            print(k, v)
|}
      in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.f <- $FuncObj(f, dummy.f, {})
      return None



  objects:
    object dummy.f:
      code:
        #b0:
          n0 <- $CallMethod($LoadMethod(kwargs, items), )
          n1 <- $GetIter(n0)
          jmp b1(n1)


        #b1(n2):
          n3 <- $NextIter(n2)
          n4 <- $HasNextIter(n3)
          if n4 then jmp b2 else jmp b3


        #b2:
          n5 <- $IterData(n3)
          k <- n5[0]
          v <- n5[1]
          n6 <- print(k, v)
          jmp b1(n2)


        #b3:
          return None





    functions:
      f -> dummy.f |}]


    let%expect_test _ =
      let source = {|
def f(z, x, y):
        pass

f(0, y=2, x=1)
        |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.f <- $FuncObj(f, dummy.f, {})
      n0 <- dummy.f(0, y= 2, x= 1)
      return None



  objects:
    object dummy.f:
      code:
        #b0:
          return None





    functions:
      f -> dummy.f |}]


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
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.C <- $ClassObj($FuncObj(C, dummy.C, {}), "C", $unknown.Exception)
      dummy.f <- $FuncObj(f, dummy.f, {})
      dummy.g <- $FuncObj(g, dummy.g, {})
      return None



  objects:
    object dummy.C:
      code:
        #b0:
          dummy.C.__module__ <- __name__
          dummy.C.__qualname__ <- "C"
          return None




    object dummy.f:
      code:
        #b0:
          throw dummy.C




    object dummy.g:
      code:
        #b0:
          n0 <- dummy.C()
          throw n0





    classes:
      C

    functions:
      C -> dummy.C
      f -> dummy.f
      g -> dummy.g |}]


    let%expect_test _ =
      let source = {|
import foo

def f():
          raise foo.bar(42)
          |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      $ImportName(foo, from_list= [])
      dummy.foo <- $ImportName(foo, from_list= [])
      dummy.f <- $FuncObj(f, dummy.f, {})
      return None



  objects:
    object dummy.f:
      code:
        #b0:
          n0 <- $CallMethod($LoadMethod(foo, bar), 42)
          throw n0





    functions:
      f -> dummy.f |}]


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
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.C <- $ClassObj($FuncObj(C, dummy.C, {}), "C")
      dummy.f <- $FuncObj(f, dummy.f, {(s, "zuck"); (y, 1); (z, 2); })
      n0 <- dummy.f(0)
      n1 <- dummy.f(10, 100)
      n2 <- dummy.f(100, 1000, 0)
      n3 <- dummy.f(0, 0, 0, "toto")
      return None



  objects:
    object dummy.C:
      code:
        #b0:
          dummy.C.__module__ <- __name__
          dummy.C.__qualname__ <- "C"
          return None




    object dummy.f:
      code:
        #b0:
          return None





    classes:
      C

    functions:
      C -> dummy.C
      f -> dummy.f |}]


    let%expect_test _ =
      let source =
        {|
# From Cinder test suite
class TestHook:
    def __init__(self, raise_on_events=None, exc_type=RuntimeError):
          pass
          |}
      in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.TestHook <- $ClassObj($FuncObj(TestHook, dummy.TestHook, {}), "TestHook")
      return None



  objects:
    object dummy.TestHook:
      code:
        #b0:
          dummy.TestHook.__module__ <- __name__
          dummy.TestHook.__qualname__ <- "TestHook"
          dummy.TestHook.__init__ <- $FuncObj(__init__, dummy.TestHook.__init__, {(exc_type, RuntimeError); (raise_on_events, None); })
          return None



      objects:
        object dummy.TestHook.__init__:
          code:
            #b0:
              return None





        functions:
          __init__ -> dummy.TestHook.__init__


    classes:
      TestHook

    functions:
      TestHook -> dummy.TestHook
          |}]


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
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.C <- $ClassObj($FuncObj(C, dummy.C, {}), "C")
      n0 <- dummy.C()
      dummy.c <- n0
      n1 <- $CallMethod($LoadMethod(dummy.c, f), 0)
      n2 <- $CallMethod($LoadMethod(dummy.c, f), 0, 1)
      n3 <- $CallMethod($LoadMethod(dummy.c, f), 0, 1, 2)
      return None



  objects:
    object dummy.C:
      code:
        #b0:
          dummy.C.__module__ <- __name__
          dummy.C.__qualname__ <- "C"
          dummy.C.f <- $FuncObj(f, dummy.C.f, {(y, 1); (z, 10); })
          return None



      objects:
        object dummy.C.f:
          code:
            #b0:
              n0 <- $Binary.Add(x, y)
              n1 <- $Binary.Add(n0, z)
              return n1





        functions:
          f -> dummy.C.f


    classes:
      C

    functions:
      C -> dummy.C |}]


    let%expect_test _ =
      let source = {|
class C:
        x : int = 0
        |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.C <- $ClassObj($FuncObj(C, dummy.C, {}), "C")
      return None



  objects:
    object dummy.C:
      code:
        #b0:
          dummy.C.__module__ <- __name__
          dummy.C.__qualname__ <- "C"
          $SETUP_ANNOTATIONS
          dummy.C.x <- 0
          dummy.C.__annotations__["x"] <- int
          return None





    classes:
      C

    functions:
      C -> dummy.C |}]


    let%expect_test _ =
      let source = {|
import dis
def f(co, s):
          dis.dis(co, file=s)
        |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      $ImportName(dis, from_list= [])
      dummy.dis <- $ImportName(dis, from_list= [])
      dummy.f <- $FuncObj(f, dummy.f, {})
      return None



  objects:
    object dummy.f:
      code:
        #b0:
          n0 <- dis.dis(co, file= s)
          return None





    functions:
      f -> dummy.f |}]


    let%expect_test _ =
      let source = {|
def f(name, args):
    return f"foo.{name!r}{name!s}{name!a}"
          |} in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.f <- $FuncObj(f, dummy.f, {})
      return None



  objects:
    object dummy.f:
      code:
        #b0:
          n0 <- $FormatFn.repr(name)
          n1 <- $Format(n0, None)
          n2 <- $FormatFn.str(name)
          n3 <- $Format(n2, None)
          n4 <- $FormatFn.ascii(name)
          n5 <- $Format(n4, None)
          return $Concat("foo.", n1, n3, n5)





    functions:
      f -> dummy.f |}]


    let%expect_test _ =
      let source =
        {|
# from Cinder
import decimal

def assertEqual(x):
  pass

def test_format_specifier_expressions(self):
  width = 10
  precision = 4
  value = decimal.Decimal('12.34567')
  assertEqual(f'result: {value:{width}.{precision}}')
        |}
      in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      $ImportName(decimal, from_list= [])
      dummy.decimal <- $ImportName(decimal, from_list= [])
      dummy.assertEqual <- $FuncObj(assertEqual, dummy.assertEqual, {})
      dummy.test_format_specifier_expressions <- $FuncObj(test_format_specifier_expressions, dummy.test_format_specifier_expressions, {})
      return None



  objects:
    object dummy.assertEqual:
      code:
        #b0:
          return None




    object dummy.test_format_specifier_expressions:
      code:
        #b0:
          width <- 10
          precision <- 4
          n0 <- $CallMethod($LoadMethod(decimal, Decimal), "12.34567")
          value <- n0
          n1 <- $Format(width, None)
          n2 <- $Format(precision, None)
          n3 <- $Format(value, $Concat(n1, ".", n2))
          n4 <- dummy.assertEqual($Concat("result: ", n3))
          return None





    functions:
      assertEqual -> dummy.assertEqual
      test_format_specifier_expressions -> dummy.test_format_specifier_expressions
          |}]


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
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.pos <- $FuncObj(pos, dummy.pos, {})
      dummy.neg <- $FuncObj(neg, dummy.neg, {})
      dummy.test_not <- $FuncObj(test_not, dummy.test_not, {})
      dummy.inv <- $FuncObj(inv, dummy.inv, {})
      return None



  objects:
    object dummy.pos:
      code:
        #b0:
          n0 <- $Unary.Positive(x)
          return n0




    object dummy.neg:
      code:
        #b0:
          n0 <- $Unary.Negative(x)
          return n0




    object dummy.test_not:
      code:
        #b0:
          n0 <- $Unary.Not(x)
          return n0




    object dummy.inv:
      code:
        #b0:
          n0 <- $Unary.Invert(x)
          return n0





    functions:
      inv -> dummy.inv
      neg -> dummy.neg
      pos -> dummy.pos
      test_not -> dummy.test_not |}]


    let%expect_test _ =
      let source =
        {|
x : int
x = 0

y : str = "zuck"


import C
z : C.T = 42

def f():
    # python bytecode doesn't keep the annotations for local variables
    u: int
    u = 0

    v: str = "tata"
        |}
      in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      $SETUP_ANNOTATIONS
      dummy.__annotations__["x"] <- int
      dummy.x <- 0
      dummy.y <- "zuck"
      dummy.__annotations__["y"] <- str
      $ImportName(C, from_list= [])
      dummy.C <- $ImportName(C, from_list= [])
      dummy.z <- 42
      dummy.__annotations__["z"] <- C.T
      dummy.f <- $FuncObj(f, dummy.f, {})
      return None



  objects:
    object dummy.f:
      code:
        #b0:
          u <- 0
          v <- "tata"
          return None





    functions:
      f -> dummy.f |}]


    let%expect_test _ =
      let source =
        {|
def f(match, it, n):
    for item in match:
        if not it[n]==item: raise AssertionError
        n+=1
          |}
      in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.f <- $FuncObj(f, dummy.f, {})
      return None



  objects:
    object dummy.f:
      code:
        #b0:
          n0 <- $GetIter(match)
          jmp b1(n0)


        #b1(n1):
          n2 <- $NextIter(n1)
          n3 <- $HasNextIter(n2)
          if n3 then jmp b2 else jmp b3


        #b2:
          n4 <- $IterData(n2)
          item <- n4
          n5 <- $Compare.eq(it[n], item)
          if $Not(n5) then jmp b4(n1) else jmp b5(n1)


        #b4(n6):
          throw AssertionError


        #b5(n7):
          n8 <- $Inplace.Add(n, 1)
          n <- n8
          jmp b1(n7)


        #b3:
          return None





    functions:
      f -> dummy.f
          |}]


    let%expect_test _ =
      let source =
        {|
def f(foo):
    for path in foo:
        if path:
                return
        |}
      in
      test source ;
      [%expect
        {|
module
object dummy:
  code:
    #b0:
      dummy.f <- $FuncObj(f, dummy.f, {})
      return None



  objects:
    object dummy.f:
      code:
        #b0:
          n0 <- $GetIter(foo)
          jmp b1(n0)


        #b1(n1):
          n2 <- $NextIter(n1)
          n3 <- $HasNextIter(n2)
          if n3 then jmp b2 else jmp b3


        #b2:
          n4 <- $IterData(n2)
          path <- n4
          if path then jmp b4(n1) else jmp b1(n1)


        #b4(n5):
          return None


        #b3:
          return None





    functions:
      f -> dummy.f
          |}]
  end )
