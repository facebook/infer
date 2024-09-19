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
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              x(PyIR.Name) <- PYCInt (42)
              return PYCNone |}]


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
            #b0 .label:
              x(PyIR.Name) <- PYCInt (42)
              n0 <- print(PyIR.Name)(x(PyIR.Name))
              return PYCNone |}]


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
            #b0 .label:
              x(PyIR.Name) <- PYCInt (42)
              y(PyIR.Name) <- PYCInt (10)
              n0 <- $Binary.Add(x(PyIR.Name), y(PyIR.Name))
              n1 <- print(PyIR.Name)(n0)
              return PYCNone |}]


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
            #b0 .label:
              x(PyIR.Name) <- PYCInt (42)
              y(PyIR.Name) <- PYCInt (10)
              n0 <- $Binary.Subtract(x(PyIR.Name), y(PyIR.Name))
              n1 <- print(PyIR.Name)(n0)
              return PYCNone |}]


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
            #b0 .label:
              x(PyIR.Name) <- PYCInt (42)
              n0 <- $Inplace.Add(x(PyIR.Name), PYCInt (10))
              x(PyIR.Name) <- n0
              n1 <- print(PyIR.Name)(x(PyIR.Name))
              return PYCNone |}]


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
            #b0 .label:
              x(PyIR.Name) <- PYCInt (42)
              n0 <- $Inplace.Subtract(x(PyIR.Name), PYCInt (10))
              x(PyIR.Name) <- n0
              n1 <- print(PyIR.Name)(x(PyIR.Name))
              return PYCNone |}]


    let%expect_test _ =
      let source = {|
pi = 3.14
      |} in
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              pi(PyIR.Name) <- PYCFloat (3.14)
              return PYCNone |}]


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
            #b0 .label:
              byte_data(PyIR.Name) <- PYCBytes ("Hello")
              return PYCNone |}]


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
            #b0 .label:
              my_fun(PyIR.Name) <- $FuncObj(my_fun, my_fun, {})
              a(PyIR.Name) <- PYCInt (10)
              n0 <- my_fun(PyIR.Name)(PYCInt (42), a(PyIR.Name))
              z(PyIR.Name) <- n0
              n1 <- print(PyIR.Name)(z(PyIR.Name))
              return PYCNone



          objects:
            object my_fun:
              code:
                #b0 .label:
                  n0 <- print(PyIR.Global)(x(PyIR.Fast))
                  n1 <- print(PyIR.Global)(y(PyIR.Fast))
                  n2 <- $Binary.Add(x(PyIR.Fast), y(PyIR.Fast))
                  z(PyIR.Fast) <- n2
                  return z(PyIR.Fast)





            functions:
              my_fun -> my_fun |}]


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
            #b0 .label:
              update_global(PyIR.Name) <- $FuncObj(update_global, update_global, {})
              z(PyIR.Global) <- PYCInt (0)
              n0 <- update_global(PyIR.Name)()
              n1 <- print(PyIR.Name)(z(PyIR.Global))
              return PYCNone



          objects:
            object update_global:
              code:
                #b0 .label:
                  n0 <- $Binary.Add(z(PyIR.Global), PYCInt (1))
                  z(PyIR.Global) <- n0
                  return PYCNone





            functions:
              update_global -> update_global |}]


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
            #b0 .label:
              coin(PyIR.Name) <- $FuncObj(coin, coin, {})
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object coin:
              code:
                #b0 .label:
                  return PYCBool (false)




            object f:
              code:
                #b0 .label:
                  n0 <- coin(PyIR.Global)()
                  if n0 then jmp b1 else jmp b2


                #b1 .label:
                  return x(PyIR.Fast)


                #b2 .label:
                  return y(PyIR.Fast)





            functions:
              coin -> coin
              f -> f |}]


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
            #b0 .label:
              coin(PyIR.Name) <- $FuncObj(coin, coin, {})
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object coin:
              code:
                #b0 .label:
                  return PYCBool (false)




            object f:
              code:
                #b0 .label:
                  z(PyIR.Fast) <- PYCInt (0)
                  n0 <- coin(PyIR.Global)()
                  if n0 then jmp b1 else jmp b2


                #b1 .label:
                  z(PyIR.Fast) <- x(PyIR.Fast)
                  jmp b3


                #b2 .label:
                  z(PyIR.Fast) <- y(PyIR.Fast)
                  jmp b3


                #b3 .label:
                  return z(PyIR.Fast)





            functions:
              coin -> coin
              f -> f |}]


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
            #b0 .label:
              coin(PyIR.Name) <- $FuncObj(coin, coin, {})
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object coin:
              code:
                #b0 .label:
                  return PYCBool (false)




            object f:
              code:
                #b0 .label:
                  z(PyIR.Fast) <- PYCInt (0)
                  n0 <- coin(PyIR.Global)()
                  if n0 then jmp b1 else jmp b2


                #b1 .label:
                  n1 <- coin(PyIR.Global)()
                  if n1 then jmp b3 else jmp b4


                #b3 .label:
                  z(PyIR.Fast) <- x(PyIR.Fast)
                  jmp b5


                #b4 .label:
                  return PYCInt (1664)


                #b5 .label:
                  n2 <- $Binary.Add(z(PyIR.Fast), PYCInt (1))
                  z(PyIR.Fast) <- n2
                  jmp b6


                #b2 .label:
                  n3 <- $Binary.Add(z(PyIR.Fast), PYCInt (1))
                  z(PyIR.Fast) <- n3
                  n4 <- coin(PyIR.Global)()
                  if n4 then jmp b7 else jmp b8


                #b7 .label:
                  return PYCInt (42)


                #b8 .label:
                  z(PyIR.Fast) <- y(PyIR.Fast)
                  jmp b6


                #b6 .label:
                  return z(PyIR.Fast)





            functions:
              coin -> coin
              f -> f |}]


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
            #b0 .label:
              foo(PyIR.Name) <- $FuncObj(foo, foo, {})
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object foo:
              code:
                #b0 .label:
                  return PYCNone




            object f:
              code:
                #b0 .label:
                  if x(PyIR.Fast) then jmp b1(foo(PyIR.Global)) else jmp b2(foo(PyIR.Global))


                #b1(n0) .label:
                  jmp b3(PYCInt (1), n0)


                #b2(n1) .label:
                  jmp b3(PYCInt (0), n1)


                #b3(n3, n2) .label:
                  n4 <- n2(n3)
                  return PYCNone





            functions:
              f -> f
              foo -> foo |}]


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
            #b0 .label:
              n0 <- range(PyIR.Name)(PYCInt (10))
              n1 <- $GetIter(n0)
              jmp b1(n1)


            #b1(n2) .label:
              n3 <- $NextIter(n2)
              n4 <- $HasNextIter(n3)
              if n4 then jmp b2 else jmp b3


            #b2 .label:
              n5 <- $IterData(n3)
              x(PyIR.Name) <- n5
              n6 <- print(PyIR.Name)(x(PyIR.Name))
              jmp b1(n2)


            #b3 .label:
              return PYCNone |}]


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
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  n0 <- $GetIter(l(PyIR.Fast))
                  jmp b1(n0)


                #b1(n1) .label:
                  n2 <- $NextIter(n1)
                  n3 <- $HasNextIter(n2)
                  if n3 then jmp b2 else jmp b3


                #b2 .label:
                  n4 <- $IterData(n2)
                  x(PyIR.Fast) <- n4
                  n5 <- bar(PyIR.Fast)()
                  n6 <- $LoadMethod(n5, __enter__)()
                  n9 <- toto(PyIR.Fast)()
                  n10 <- $LoadMethod(n9, __enter__)()
                  obj(PyIR.Fast) <- n10
                  if y(PyIR.Fast) then jmp b6(CM(n9).__exit__, CM(n5).__exit__, n1) else
                  jmp b7(CM(n9).__exit__, CM(n5).__exit__, n1)


                #b6(n16, n15, n14) .label:
                  jmp b8(n16, n15, n14)


                #b8(n22, n21, n20) .finally:
                  n23 <- n22(PYCNone, PYCNone, PYCNone)
                  jmp b9(n21, n20)


                #b9(n25, n24) .finally:
                  n26 <- n25(PYCNone, PYCNone, PYCNone)
                  jmp b1(n24)


                #b7(n19, n18, n17) .label:
                  n28 <- print(PyIR.Global)(PYCString ("nop"))
                  jmp b5(n19, n18, n17)


                #b5(n13, n12, n11) .finally:
                  n32 <- n13(PYCNone, PYCNone, PYCNone)
                  jmp b4(n12, n11)


                #b4(n8, n7) .finally:
                  n35 <- n8(PYCNone, PYCNone, PYCNone)
                  jmp b1(n7)


                #b3 .label:
                  return PYCNone





            functions:
              f -> f |}]


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
            #b0 .label:
              l(PyIR.Name) <- [PYCInt (0), PYCInt (1), PYCInt (2), PYCInt (3), PYCInt (4), PYCInt (5)]
              n0 <- l(PyIR.Name)[[PYCInt (0):PYCInt (2)]]
              n1 <- l(PyIR.Name)[[PYCInt (0):PYCInt (2):PYCInt (1)]]
              return PYCNone |}]


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
            #b0 .label:
              n0 <- print(PyIR.Name)(PYCInt (42))
              print(PyIR.Name) <- $FuncObj(print, print, {})
              n1 <- print(PyIR.Name)(PYCInt (42))
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object print:
              code:
                #b0 .label:
                  return x(PyIR.Fast)




            object f:
              code:
                #b0 .label:
                  n0 <- print(PyIR.Global)(x(PyIR.Fast))
                  return PYCNone





            functions:
              f -> f
              print -> print |}]


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
            #b0 .label:
              f0(PyIR.Name) <- $FuncObj(f0, f0, {})
              f1(PyIR.Name) <- $FuncObj(f1, f1, {})
              return PYCNone



          objects:
            object f0:
              code:
                #b0 .label:
                  return PYCNone




            object f1:
              code:
                #b0 .label:
                  return PYCNone





            functions:
              f0 -> f0
              f1 -> f1 |}]


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
            #b0 .label:
              expect_int(PyIR.Name) <- $FuncObj(expect_int, expect_int, {})
              get(PyIR.Name) <- $FuncObj(get, get, {})
              n0 <- get(PyIR.Name)()
              n1 <- expect_int(PyIR.Name)(n0)
              return PYCNone



          objects:
            object expect_int:
              code:
                #b0 .label:
                  return PYCNone




            object get:
              code:
                #b0 .label:
                  return PYCInt (42)





            functions:
              expect_int -> expect_int
              get -> get |}]


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
            #b0 .label:
              expect(PyIR.Name) <- $FuncObj(expect, expect, {})
              get(PyIR.Name) <- $FuncObj(get, get, {})
              n0 <- get(PyIR.Name)()
              n1 <- expect(PyIR.Name)(n0)
              return PYCNone



          objects:
            object expect:
              code:
                #b0 .label:
                  return PYCNone




            object get:
              code:
                #b0 .label:
                  return PYCInt (42)





            functions:
              expect -> expect
              get -> get |}]


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
            #b0 .label:
              n0 <- $BuildClass($FuncObj(C, C, {}), PYCString ("C"))
              C(PyIR.Name) <- n0
              n1 <- C(PyIR.Name)(PYCInt (0), PYCString ("a"))
              c(PyIR.Name) <- n1
              n2 <- c(PyIR.Name).x
              n3 <- $CallMethod($LoadMethod(c(PyIR.Name), get), )
              n4 <- $CallMethod($LoadMethod(c(PyIR.Name), set), PYCInt (42))
              return PYCNone



          objects:
            object C:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("C")
                  __init__(PyIR.Name) <- $FuncObj(__init__, C.__init__, {})
                  get(PyIR.Name) <- $FuncObj(get, C.get, {})
                  set(PyIR.Name) <- $FuncObj(set, C.set, {})
                  return PYCNone



              objects:
                object __init__:
                  code:
                    #b0 .label:
                      self(PyIR.Fast).x <- x(PyIR.Fast)
                      self(PyIR.Fast).y <- y(PyIR.Fast)
                      return PYCNone




                object get:
                  code:
                    #b0 .label:
                      return self(PyIR.Fast).x




                object set:
                  code:
                    #b0 .label:
                      self(PyIR.Fast).x <- x(PyIR.Fast)
                      return PYCNone





                functions:
                  __init__ -> C.__init__
                  get -> C.get
                  set -> C.set


                functions:
                  C -> C |}]


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
            #b0 .label:
              n0 <- $BuildClass($FuncObj(IntBox, IntBox, {}), PYCString ("IntBox"))
              IntBox(PyIR.Name) <- n0
              getX(PyIR.Name) <- $FuncObj(getX, getX, {})
              n1 <- IntBox(PyIR.Name)(PYCInt (10))
              c(PyIR.Name) <- n1
              n2 <- c(PyIR.Name).x
              c(PyIR.Name).z <- PYCInt (10)
              n3 <- $CallMethod($LoadMethod(c(PyIR.Name), get), )
              n4 <- $CallMethod($LoadMethod(c(PyIR.Name), set), PYCInt (42))
              n5 <- $CallMethod($LoadMethod(c(PyIR.Name), run), )
              n6 <- print(PyIR.Name)(c(PyIR.Name).z)
              return PYCNone



          objects:
            object IntBox:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("IntBox")
                  $SETUP_ANNOTATIONS
                  __annotations__(PyIR.Name)[PYCString ("x")] <- int(PyIR.Name)
                  __init__(PyIR.Name) <- $FuncObj(__init__, IntBox.__init__, {})
                  get(PyIR.Name) <- $FuncObj(get, IntBox.get, {})
                  set(PyIR.Name) <- $FuncObj(set, IntBox.set, {})
                  run(PyIR.Name) <- $FuncObj(run, IntBox.run, {})
                  n0 <- staticmethod(PyIR.Name)($FuncObj(id, IntBox.id, {}))
                  id(PyIR.Name) <- n0
                  return PYCNone



              objects:
                object __init__:
                  code:
                    #b0 .label:
                      self(PyIR.Fast).x <- x(PyIR.Fast)
                      return PYCNone




                object get:
                  code:
                    #b0 .label:
                      return self(PyIR.Fast).x




                object set:
                  code:
                    #b0 .label:
                      self(PyIR.Fast).x <- x(PyIR.Fast)
                      return PYCNone




                object run:
                  code:
                    #b0 .label:
                      return PYCNone




                object id:
                  code:
                    #b0 .label:
                      return x(PyIR.Fast)





                functions:
                  __init__ -> IntBox.__init__
                  get -> IntBox.get
                  id -> IntBox.id
                  run -> IntBox.run
                  set -> IntBox.set

                object getX:
                  code:
                    #b0 .label:
                      n0 <- $CallMethod($LoadMethod(box(PyIR.Fast), get), )
                      return n0





                functions:
                  IntBox -> IntBox
                  getX -> getX |}]


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
            #b0 .label:
              n0 <- $BuildClass($FuncObj(C, C, {}), PYCString ("C"))
              C(PyIR.Name) <- n0
              n1 <- $BuildClass($FuncObj(D, D, {}), PYCString ("D"), C(PyIR.Name))
              D(PyIR.Name) <- n1
              return PYCNone



          objects:
            object C:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("C")
                  n0 <- staticmethod(PyIR.Name)($FuncObj(f, C.f, {}))
                  f(PyIR.Name) <- n0
                  n1 <- staticmethod(PyIR.Name)($FuncObj(typed_f, C.typed_f, {}))
                  typed_f(PyIR.Name) <- n1
                  return PYCNone



              objects:
                object f:
                  code:
                    #b0 .label:
                      return PYCNone




                object typed_f:
                  code:
                    #b0 .label:
                      return x(PyIR.Fast)





                functions:
                  f -> C.f
                  typed_f -> C.typed_f

                object D:
                  code:
                    #b0 .label:
                      __module__(PyIR.Name) <- __name__(PyIR.Name)
                      __qualname__(PyIR.Name) <- PYCString ("D")
                      return PYCNone





                functions:
                  C -> C
                  D -> D |}]


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
            #b0 .label:
              n0 <- $BuildClass($FuncObj(C, C, {}), PYCString ("C"))
              C(PyIR.Name) <- n0
              n1 <- $CallMethod($LoadMethod(C(PyIR.Name), f), )
              return PYCNone



          objects:
            object C:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("C")
                  n0 <- staticmethod(PyIR.Name)($FuncObj(f, C.f, {}))
                  f(PyIR.Name) <- n0
                  return PYCNone



              objects:
                object f:
                  code:
                    #b0 .label:
                      return PYCNone





                functions:
                  f -> C.f


            functions:
              C -> C |}]


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
            #b0 .label:
              n0 <- $BuildClass($FuncObj(A, A, {}), PYCString ("A"))
              A(PyIR.Name) <- n0
              n1 <- $BuildClass($FuncObj(C, C, {}), PYCString ("C"))
              C(PyIR.Name) <- n1
              g(PyIR.Name) <- $FuncObj(g, g, {})
              return PYCNone



          objects:
            object A:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("A")
                  f(PyIR.Name) <- $FuncObj(f, A.f, {})
                  return PYCNone



              objects:
                object f:
                  code:
                    #b0 .label:
                      return PYCNone





                functions:
                  f -> A.f

              object C:
                code:
                  #b0 .label:
                    __module__(PyIR.Name) <- __name__(PyIR.Name)
                    __qualname__(PyIR.Name) <- PYCString ("C")
                    $SETUP_ANNOTATIONS
                    __annotations__(PyIR.Name)[PYCString ("a")] <- A(PyIR.Name)
                    return PYCNone




              object g:
                code:
                  #b0 .label:
                    n0 <- $CallMethod($LoadMethod(c(PyIR.Fast).a, f), )
                    n1 <- print(PyIR.Global)(n0)
                    return PYCNone





              functions:
                A -> A
                C -> C
                g -> g |}]


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
            #b0 .label:
              n0 <- $BuildClass($FuncObj(A, A, {}), PYCString ("A"))
              A(PyIR.Name) <- n0
              n1 <- $BuildClass($FuncObj(B, B, {}), PYCString ("B"))
              B(PyIR.Name) <- n1
              n2 <- $BuildClass($FuncObj(C, C, {}), PYCString ("C"), A(PyIR.Name), B(PyIR.Name))
              C(PyIR.Name) <- n2
              return PYCNone



          objects:
            object A:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("A")
                  return PYCNone




            object B:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("B")
                  return PYCNone




            object C:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("C")
                  return PYCNone





            functions:
              A -> A
              B -> B
              C -> C |}]


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
            #b0 .label:
              n0 <- $BuildClass($FuncObj(C, C, {}), PYCString ("C"))
              C(PyIR.Name) <- n0
              build(PyIR.Name) <- $FuncObj(build, build, {})
              n1 <- build(PyIR.Name)()
              cs(PyIR.Name) <- n1
              n2 <- cs(PyIR.Name)[PYCInt (0)].x
              return PYCNone



          objects:
            object C:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("C")
                  __init__(PyIR.Name) <- $FuncObj(__init__, C.__init__, {})
                  return PYCNone



              objects:
                object __init__:
                  code:
                    #b0 .label:
                      self(PyIR.Fast).x <- PYCInt (0)
                      return PYCNone





                functions:
                  __init__ -> C.__init__

              object build:
                code:
                  #b0 .label:
                    n0 <- C(PyIR.Global)()
                    return [n0]





              functions:
                C -> C
                build -> build |}]


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
            #b0 .label:
              f(PyIR.Name) <- $FuncObj(f, f, {})
              n0 <- f(PyIR.Name)()
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  n0 <- $BuildClass($FuncObj(A, A, {}), PYCString ("A"))
                  A(PyIR.Fast) <- n0
                  n1 <- A(PyIR.Fast)()
                  a(PyIR.Fast) <- n1
                  n2 <- $CallMethod($LoadMethod(a(PyIR.Fast), get), )
                  return n2



              objects:
                object A:
                  code:
                    #b0 .label:
                      __module__(PyIR.Name) <- __name__(PyIR.Name)
                      __qualname__(PyIR.Name) <- PYCString ("f.<locals>.A")
                      __init__(PyIR.Name) <- $FuncObj(__init__, f.<locals>.A.__init__, {})
                      get(PyIR.Name) <- $FuncObj(get, f.<locals>.A.get, {})
                      return PYCNone



                  objects:
                    object __init__:
                      code:
                        #b0 .label:
                          self(PyIR.Fast).x <- PYCInt (0)
                          return PYCNone




                    object get:
                      code:
                        #b0 .label:
                          return self(PyIR.Fast).x





                    functions:
                      __init__ -> f.<locals>.A.__init__
                      get -> f.<locals>.A.get


                  functions:
                    A -> A


                functions:
                  f -> f |}]


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
            #b0 .label:
              $ImportName(base, from_list= [])
              base(PyIR.Name) <- $ImportName(base, from_list= [])
              $ImportName(base, from_list= [])
              base(PyIR.Name) <- $ImportName(base, from_list= [])
              n0 <- $CallMethod($LoadMethod(base(PyIR.Name), f), PYCInt (0))
              return PYCNone |}]


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
            #b0 .label:
              f(PyIR.Name) <- $FuncObj(f, f, {})
              n0 <- f(PyIR.Name)()
              $ImportName(base, from_list= [f, g])
              f(PyIR.Name) <- $ImportFrom($ImportName(base, from_list= [f, g]), name= f)
              g(PyIR.Name) <- $ImportFrom($ImportName(base, from_list= [f, g]), name= g)
              n1 <- f(PyIR.Name)()
              $ImportName(base, from_list= [f, g])
              f(PyIR.Name) <- $ImportFrom($ImportName(base, from_list= [f, g]), name= f)
              g(PyIR.Name) <- $ImportFrom($ImportName(base, from_list= [f, g]), name= g)
              n2 <- g(PyIR.Name)()
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  return PYCNone





            functions:
              f -> f |}]


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
            #b0 .label:
              $ImportName(unittest, from_list= [])
              unittest(PyIR.Name) <- $ImportName(unittest, from_list= [])
              n0 <- $BuildClass($FuncObj(MyTest, MyTest, {}), PYCString ("MyTest"), unittest(PyIR.Name).TestCase)
              MyTest(PyIR.Name) <- n0
              return PYCNone



          objects:
            object MyTest:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("MyTest")
                  return PYCNone





            functions:
              MyTest -> MyTest |}]


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
            #b0 .label:
              $ImportName(os, from_list= [])
              os(PyIR.Name) <- $ImportName(os, from_list= [])
              $ImportName(sys, from_list= [])
              sys(PyIR.Name) <- $ImportName(sys, from_list= [])
              $ImportName(test.libregrtest, from_list= [main])
              main(PyIR.Name) <- $ImportFrom($ImportName(test.libregrtest, from_list= [main]), name= main)
              main_in_temp_cwd(PyIR.Name) <- main(PyIR.Name)
              _main(PyIR.Name) <- $FuncObj(_main, _main, {})
              n0 <- $Compare.eq(__name__(PyIR.Name), PYCString ("__main__"))
              if n0 then jmp b1 else jmp b2


            #b1 .label:
              n1 <- _main(PyIR.Name)()
              jmp b2


            #b2 .label:
              return PYCNone



          objects:
            object _main:
              code:
                #b0 .label:
                  n0 <- $CallMethod($LoadMethod(os(PyIR.Global).path, dirname),
                    sys(PyIR.Global).argv[PYCInt (0)])
                  n1 <- $CallMethod($LoadMethod(os(PyIR.Global).path, normpath), n0)
                  n2 <- $CallMethod($LoadMethod(os(PyIR.Global).path, abspath), n1)
                  mydir(PyIR.Fast) <- n2
                  n3 <- len(PyIR.Global)(sys(PyIR.Global).path)
                  n4 <- $Binary.Subtract(n3, PYCInt (1))
                  i(PyIR.Fast) <- n4
                  jmp b1


                #b1 .label:
                  n5 <- $Compare.ge(i(PyIR.Fast), PYCInt (0))
                  if n5 then jmp b2 else jmp b3


                #b2 .label:
                  n6 <- $CallMethod($LoadMethod(os(PyIR.Global).path, normpath),
                    sys(PyIR.Global).path[i(PyIR.Fast)])
                  n7 <- $CallMethod($LoadMethod(os(PyIR.Global).path, abspath), n6)
                  n8 <- $Compare.eq(n7, mydir(PyIR.Fast))
                  if n8 then jmp b4 else jmp b5


                #b4 .label:
                  jmp b1


                #b5 .label:
                  n9 <- $Inplace.Subtract(i(PyIR.Fast), PYCInt (1))
                  i(PyIR.Fast) <- n9
                  jmp b1


                #b3 .label:
                  n10 <- $CallMethod($LoadMethod(os(PyIR.Global).path, abspath), __file__(PyIR.Global))
                  __file__(PyIR.Global) <- n10
                  n11 <- main(PyIR.Global)()
                  return PYCNone





            functions:
              _main -> _main |}]


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
        object some/long/path/dummy:
          code:
            #b0 .label:
              $ImportName(A, from_list= [X])
              X(PyIR.Name) <- $ImportFrom($ImportName(A, from_list= [X]), name= X)
              n0 <- X(PyIR.Name)()
              $ImportName(B, from_list= [X])
              X(PyIR.Name) <- $ImportFrom($ImportName(B, from_list= [X]), name= X)
              n1 <- X(PyIR.Name)()
              $ImportName(C, from_list= [X])
              X(PyIR.Name) <- $ImportFrom($ImportName(C, from_list= [X]), name= X)
              n2 <- X(PyIR.Name)()
              $ImportName(, from_list= [path])
              path(PyIR.Name) <- $ImportFrom($ImportName(, from_list= [path]), name= path)
              n3 <- $CallMethod($LoadMethod(path(PyIR.Name), X), )
              return PYCNone |}]


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
            #b0 .label:
              $ImportName(x, from_list= [y, a])
              z(PyIR.Name) <- $ImportFrom($ImportName(x, from_list= [y, a]), name= y)
              b(PyIR.Name) <- $ImportFrom($ImportName(x, from_list= [y, a]), name= a)
              $ImportName(x, from_list= [y, a])
              z(PyIR.Name) <- $ImportFrom($ImportName(x, from_list= [y, a]), name= y)
              b(PyIR.Name) <- $ImportFrom($ImportName(x, from_list= [y, a]), name= a)
              n0 <- z(PyIR.Name)()
              n1 <- b(PyIR.Name)()
              $ImportName(foo, from_list= [toto, tata])
              toto(PyIR.Name) <- $ImportFrom($ImportName(foo, from_list= [toto, tata]), name= toto)
              tata(PyIR.Name) <- $ImportFrom($ImportName(foo, from_list= [toto, tata]), name= tata)
              $ImportName(foo, from_list= [toto, tata])
              toto(PyIR.Name) <- $ImportFrom($ImportName(foo, from_list= [toto, tata]), name= toto)
              tata(PyIR.Name) <- $ImportFrom($ImportName(foo, from_list= [toto, tata]), name= tata)
              n2 <- toto(PyIR.Name)()
              n3 <- tata(PyIR.Name)()
              return PYCNone |}]


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
            #b0 .label:
              $ImportName(xml.etree.ElementTree, from_list= [])
              ET(PyIR.Name) <- $ImportFrom($ImportName(xml.etree.ElementTree, from_list= []),
                name= ElementTree)
              return PYCNone |}]


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
            #b0 .label:
              n0 <- $BuildClass($FuncObj(C, C, {}), PYCString ("C"))
              C(PyIR.Name) <- n0
              n1 <- $BuildClass($FuncObj(D, D, {}), PYCString ("D"), C(PyIR.Name))
              D(PyIR.Name) <- n1
              return PYCNone



          objects:
            object C:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("C")
                  return PYCNone




            object D:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("D")
                  return PYCNone





            functions:
              C -> C
              D -> D |}]


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
            #b0 .label:
              n0 <- $BuildClass($FuncObj(C, C, {}), PYCString ("C"))
              C(PyIR.Name) <- n0
              n1 <- $BuildClass($FuncObj(D, D, {}), PYCString ("D"), C(PyIR.Name))
              D(PyIR.Name) <- n1
              n2 <- $BuildClass($FuncObj(C0, C0, {}), PYCString ("C0"))
              C0(PyIR.Name) <- n2
              n3 <- $BuildClass($FuncObj(D0, D0, {}), PYCString ("D0"), C0(PyIR.Name))
              D0(PyIR.Name) <- n3
              return PYCNone



          objects:
            object C:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("C")
                  return PYCNone




            object D:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("D")
                  __init__(PyIR.Name) <- $FuncObj(__init__, D.__init__, {})
                  __classcell__(PyIR.Name) <- $Ref(__class__)
                  return $Ref(__class__)



              objects:
                object __init__:
                  code:
                    #b0 .label:
                      n0 <- super(PyIR.Global)()
                      n1 <- $CallMethod($LoadMethod(n0, __init__), )
                      return PYCNone





                functions:
                  __init__ -> D.__init__

              object C0:
                code:
                  #b0 .label:
                    __module__(PyIR.Name) <- __name__(PyIR.Name)
                    __qualname__(PyIR.Name) <- PYCString ("C0")
                    __init__(PyIR.Name) <- $FuncObj(__init__, C0.__init__, {})
                    return PYCNone



                objects:
                  object __init__:
                    code:
                      #b0 .label:
                        foo(PyIR.Fast).x <- x(PyIR.Fast)
                        return PYCNone





                  functions:
                    __init__ -> C0.__init__

                object D0:
                  code:
                    #b0 .label:
                      __module__(PyIR.Name) <- __name__(PyIR.Name)
                      __qualname__(PyIR.Name) <- PYCString ("D0")
                      __init__(PyIR.Name) <- $FuncObj(__init__, D0.__init__, {})
                      __classcell__(PyIR.Name) <- $Ref(__class__)
                      return $Ref(__class__)



                  objects:
                    object __init__:
                      code:
                        #b0 .label:
                          n0 <- super(PyIR.Global)()
                          n1 <- $CallMethod($LoadMethod(n0, __init__), PYCInt (42))
                          return PYCNone





                    functions:
                      __init__ -> D0.__init__


                functions:
                  C -> C
                  C0 -> C0
                  D -> D
                  D0 -> D0 |}]


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
            #b0 .label:
              $ImportName(foo, from_list= [])
              foo(PyIR.Name) <- $ImportName(foo, from_list= [])
              n0 <- $BuildClass($FuncObj(C, C, {}), PYCString ("C"), foo(PyIR.Name).D)
              C(PyIR.Name) <- n0
              return PYCNone



          objects:
            object C:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("C")
                  __init__(PyIR.Name) <- $FuncObj(__init__, C.__init__, {})
                  __classcell__(PyIR.Name) <- $Ref(__class__)
                  return $Ref(__class__)



              objects:
                object __init__:
                  code:
                    #b0 .label:
                      n0 <- super(PyIR.Global)()
                      n1 <- $CallMethod($LoadMethod(n0, __init__), x(PyIR.Fast))
                      return PYCNone





                functions:
                  __init__ -> C.__init__


            functions:
              C -> C |}]


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
            #b0 .label:
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  n0 <- $Compare.eq(x(PyIR.Fast), y(PyIR.Fast))
                  return n0





            functions:
              f -> f |}]


    let%expect_test _ =
      let source = "True != False" in
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              n0 <- $Compare.neq(PYCBool (true), PYCBool (false))
              return PYCNone |}]


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
            #b0 .label:
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  if x(PyIR.Fast) then jmp b1 else jmp b2


                #b1 .label:
                  if $Not(y(PyIR.Fast)) then jmp b2 else jmp b3(y(PyIR.Fast))


                #b2 .label:
                  if z(PyIR.Fast) then jmp b4 else jmp b3(z(PyIR.Fast))


                #b4 .label:
                  jmp b3(t(PyIR.Fast))


                #b3(n1) .label:
                  return n1





            functions:
              f -> f |}]


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
            #b0 .label:
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  n0 <- $Compare.gt(x(PyIR.Fast), y(PyIR.Fast))
                  return n0





            functions:
              f -> f |}]


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
            #b0 .label:
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  n0 <- $Compare.le(x(PyIR.Fast), y(PyIR.Fast))
                  return n0





            functions:
              f -> f |}]


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
            #b0 .label:
              is_check(PyIR.Name) <- $FuncObj(is_check, is_check, {})
              is_not_check(PyIR.Name) <- $FuncObj(is_not_check, is_not_check, {})
              in_check(PyIR.Name) <- $FuncObj(in_check, in_check, {})
              in_not_check(PyIR.Name) <- $FuncObj(in_not_check, in_not_check, {})
              return PYCNone



          objects:
            object is_check:
              code:
                #b0 .label:
                  n0 <- $Compare.is(x(PyIR.Fast), PYCNone)
                  return n0




            object is_not_check:
              code:
                #b0 .label:
                  n0 <- $Compare.is_not(x(PyIR.Fast), PYCNone)
                  return n0




            object in_check:
              code:
                #b0 .label:
                  n0 <- $Compare.in(x(PyIR.Fast), l(PyIR.Fast))
                  return n0




            object in_not_check:
              code:
                #b0 .label:
                  n0 <- $Compare.not_in(x(PyIR.Fast), l(PyIR.Fast))
                  return n0





            functions:
              in_check -> in_check
              in_not_check -> in_not_check
              is_check -> is_check
              is_not_check -> is_not_check |}]


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
            #b0 .label:
              $ImportName(abc, from_list= [ABC, abstractmethod])
              ABC(PyIR.Name) <- $ImportFrom($ImportName(abc, from_list= [ABC, abstractmethod]), name= ABC)
              abstractmethod(PyIR.Name) <- $ImportFrom($ImportName(abc, from_list= [ABC, abstractmethod]),
                name= abstractmethod)
              n0 <- $BuildClass($FuncObj(C, C, {}), PYCString ("C"), ABC(PyIR.Name))
              C(PyIR.Name) <- n0
              return PYCNone



          objects:
            object C:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("C")
                  n0 <- abstractmethod(PyIR.Name)($FuncObj(get, C.get, {}))
                  get(PyIR.Name) <- n0
                  n1 <- staticmethod(PyIR.Name)($FuncObj(get_static0, C.get_static0, {}))
                  n2 <- abstractmethod(PyIR.Name)(n1)
                  get_static0(PyIR.Name) <- n2
                  n3 <- abstractmethod(PyIR.Name)($FuncObj(get_static1, C.get_static1, {}))
                  n4 <- staticmethod(PyIR.Name)(n3)
                  get_static1(PyIR.Name) <- n4
                  return PYCNone



              objects:
                object get:
                  code:
                    #b0 .label:
                      return PYCNone




                object get_static0:
                  code:
                    #b0 .label:
                      return PYCNone




                object get_static1:
                  code:
                    #b0 .label:
                      return PYCNone





                functions:
                  get -> C.get
                  get_static0 -> C.get_static0
                  get_static1 -> C.get_static1


                functions:
                  C -> C |}]


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
            #b0 .label:
              l(PyIR.Name) <- [PYCInt (1), PYCInt (2), PYCInt (3)]
              n0 <- print(PyIR.Name)(l(PyIR.Name)[PYCInt (0)])
              return PYCNone |}]


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
            #b0 .label:
              l(PyIR.Name) <- [PYCInt (1), PYCInt (2), PYCInt (3)]
              x(PyIR.Name) <- PYCInt (0)
              l(PyIR.Name)[x(PyIR.Name)] <- PYCInt (10)
              return PYCNone |}]


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
            #b0 .label:
              t(PyIR.Name) <- PYCTuple ([|PYCInt (1); PYCInt (2); PYCInt (3)|])
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  return (x(PyIR.Fast), y(PyIR.Fast), z(PyIR.Fast))





            functions:
              f -> f |}]


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
            #b0 .label:
              s(PyIR.Name) <- {PYCInt (1), PYCInt (2), PYCInt (3)}
              return PYCNone |}]


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
            #b0 .label:
              l(PyIR.Name) <- [PYCInt (1), PYCInt (2), PYCInt (3)]
              n0 <- print(PyIR.Name)(l(PyIR.Name))
              build_list(PyIR.Name) <- $FuncObj(build_list, build_list, {})
              return PYCNone



          objects:
            object build_list:
              code:
                #b0 .label:
                  return [PYCInt (1), PYCInt (2), PYCInt (3)]





            functions:
              build_list -> build_list |}]


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
            #b0 .label:
              x(PyIR.Name) <- PYCString ("1")
              s(PyIR.Name) <- {|x(PyIR.Name), PYCInt (1), PYCString ("2"), PYCInt (2)|}
              n0 <- print(PyIR.Name)(s(PyIR.Name))
              s(PyIR.Name) <- {PYCString ("a"): PYCInt (42), PYCString ("b"): PYCInt (1664), }
              n1 <- print(PyIR.Name)(s(PyIR.Name)[PYCString ("1")])
              d(PyIR.Name) <- {PYCInt (1): PYCNone, PYCInt (120): PYCString ("abc"),
                               PYCInt (121): PYCString (""), PYCBytes ("abc"):
                               PYCInt (120), }
              return PYCNone |xxx}]


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
            #b0 .label:
              $ImportName(unittest, from_list= [])
              unittest(PyIR.Name) <- $ImportName(unittest, from_list= [])
              $ImportName(signal, from_list= [])
              signal(PyIR.Name) <- $ImportName(signal, from_list= [])
              n0 <- hasattr(PyIR.Name)(signal(PyIR.Name), PYCString ("setitimer"))
              n1 <- $CallMethod($LoadMethod(unittest(PyIR.Name), skipUnless),
                n0, PYCString ("requires setitimer()"))
              n2 <- $BuildClass($FuncObj(Test, Test, {}), PYCString ("Test"), unittest(PyIR.Name).TestCase)
              n3 <- n1(n2)
              Test(PyIR.Name) <- n3
              return PYCNone



          objects:
            object Test:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("Test")
                  return PYCNone





            functions:
              Test -> Test |}]


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
            #b0 .label:
              n0 <- $BuildClass($FuncObj(C, C, {}), PYCString ("C"))
              C(PyIR.Name) <- n0
              return PYCNone



          objects:
            object C:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("C")
                  n0 <- foo(PyIR.Name)(x(PyIR.Name), y(PyIR.Name), z(PyIR.Name))
                  n1 <- n0($FuncObj(f, C.f, {}))
                  f(PyIR.Name) <- n1
                  n2 <- $CallMethod($LoadMethod(foo(PyIR.Name), bar), x(PyIR.Name), y(PyIR.Name), z(PyIR.Name))
                  n3 <- n2($FuncObj(g, C.g, {}))
                  g(PyIR.Name) <- n3
                  return PYCNone



              objects:
                object f:
                  code:
                    #b0 .label:
                      return PYCNone




                object g:
                  code:
                    #b0 .label:
                      return PYCNone





                functions:
                  f -> C.f
                  g -> C.g


              functions:
                C -> C |}]


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
            #b0 .label:
              $ImportName(unittest, from_list= [])
              unittest(PyIR.Name) <- $ImportName(unittest, from_list= [])
              n0 <- $BuildClass($FuncObj(PwdTest, PwdTest, {}), PYCString ("PwdTest"), unittest(PyIR.Name).TestCase)
              PwdTest(PyIR.Name) <- n0
              return PYCNone



          objects:
            object PwdTest:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("PwdTest")
                  test_values(PyIR.Name) <- $FuncObj(test_values, PwdTest.test_values, {})
                  return PYCNone



              objects:
                object test_values:
                  code:
                    #b0 .label:
                      n0 <- type(PyIR.Global)(e(PyIR.Fast).pw_gecos)
                      n1 <- type(PyIR.Global)(PYCNone)
                      n2 <- $CallMethod($LoadMethod(self(PyIR.Fast), assertIn), n0, (str(PyIR.Global), n1))
                      return PYCNone





                functions:
                  test_values -> PwdTest.test_values


            functions:
              PwdTest -> PwdTest |}]


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
            #b0 .label:
              n0 <- open(PyIR.Name)(PYCString ("foo.txt"), PYCString ("wt"))
              fp(PyIR.Name) <- n0
              n1 <- $CallMethod($LoadMethod(fp(PyIR.Name), write), PYCString ("yolo"))
              return PYCNone |}]


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
            #b0 .label:
              n0 <- open(PyIR.Name)(PYCString ("foo.txt"), PYCString ("wt"))
              n1 <- $LoadMethod(n0, __enter__)()
              fp(PyIR.Name) <- n1
              n3 <- $CallMethod($LoadMethod(fp(PyIR.Name), write), PYCString ("yolo"))
              jmp b1(CM(n0).__exit__)


            #b1(n2) .finally:
              n5 <- n2(PYCNone, PYCNone, PYCNone)
              return PYCNone |}]


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
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  n0 <- foo(PyIR.Fast)()
                  n1 <- $LoadMethod(n0, __enter__)()
                  foo0(PyIR.Fast) <- n1
                  n3 <- bar(PyIR.Fast)()
                  n4 <- $LoadMethod(n3, __enter__)()
                  bar0(PyIR.Fast) <- n4
                  n7 <- print(PyIR.Global)(bar0(PyIR.Fast))
                  jmp b2(CM(n3).__exit__, CM(n0).__exit__)


                #b2(n6, n5) .finally:
                  n10 <- n6(PYCNone, PYCNone, PYCNone)
                  n11 <- print(PyIR.Global)(foo0(PyIR.Fast))
                  jmp b3(n5)


                #b3(n12) .finally:
                  n13 <- n12(PYCNone, PYCNone, PYCNone)
                  return PYCInt (42)


                #b1(n2) .finally:
                  n14 <- n2(PYCNone, PYCNone, PYCNone)
                  return PYCNone





            functions:
              f -> f |}]


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
            #b0 .label:
              n0 <- print(PyIR.Name)(PYCString ("TRY BLOCK"))
              jmp b1


            #b1 .finally:
              n1 <- print(PyIR.Name)(PYCString ("FINALLY BLOCK"))
              return PYCNone |}]


    let%expect_test _ =
      let source =
        {|
try:
      print("TRY BLOCK")
finally:
      if foo:
          print("X")
      else:
          print("Y")
      print("FINALLY BLOCK")
print("END")
          |}
      in
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              n0 <- print(PyIR.Name)(PYCString ("TRY BLOCK"))
              jmp b1


            #b1 .finally:
              if foo(PyIR.Name) then jmp b2 else jmp b3


            #b2 .label:
              n1 <- print(PyIR.Name)(PYCString ("X"))
              jmp b4


            #b3 .label:
              n2 <- print(PyIR.Name)(PYCString ("Y"))
              jmp b4


            #b4 .label:
              n3 <- print(PyIR.Name)(PYCString ("FINALLY BLOCK"))
              n4 <- print(PyIR.Name)(PYCString ("END"))
              return PYCNone |}]


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
            #b0 .label:
              f(PyIR.Name) <- $FuncObj(f, f, {})
              n0 <- f(PyIR.Name)()
              a(PyIR.Name) <- n0[PYCInt (0)]
              b(PyIR.Name) <- n0[PYCInt (1)]
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  return PYCNone





            functions:
              f -> f |}]


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
            #b0 .label:
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  n0 <- $CallMethod($LoadMethod(kwargs(PyIR.Fast), items), )
                  n1 <- $GetIter(n0)
                  jmp b1(n1)


                #b1(n2) .label:
                  n3 <- $NextIter(n2)
                  n4 <- $HasNextIter(n3)
                  if n4 then jmp b2 else jmp b3


                #b2 .label:
                  n5 <- $IterData(n3)
                  k(PyIR.Fast) <- n5[PYCInt (0)]
                  v(PyIR.Fast) <- n5[PYCInt (1)]
                  n6 <- print(PyIR.Global)(k(PyIR.Fast), v(PyIR.Fast))
                  jmp b1(n2)


                #b3 .label:
                  return PYCNone





            functions:
              f -> f |}]


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
            #b0 .label:
              f(PyIR.Name) <- $FuncObj(f, f, {})
              n0 <- f(PyIR.Name)(PYCInt (0), y= PYCInt (2), x= PYCInt (1))
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  return PYCNone





            functions:
              f -> f |}]


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
            #b0 .label:
              n0 <- $BuildClass($FuncObj(C, C, {}), PYCString ("C"), Exception(PyIR.Name))
              C(PyIR.Name) <- n0
              f(PyIR.Name) <- $FuncObj(f, f, {})
              g(PyIR.Name) <- $FuncObj(g, g, {})
              return PYCNone



          objects:
            object C:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("C")
                  return PYCNone




            object f:
              code:
                #b0 .label:
                  throw C(PyIR.Global)




            object g:
              code:
                #b0 .label:
                  n0 <- C(PyIR.Global)()
                  throw n0





            functions:
              C -> C
              f -> f
              g -> g |}]


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
            #b0 .label:
              $ImportName(foo, from_list= [])
              foo(PyIR.Name) <- $ImportName(foo, from_list= [])
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  n0 <- $CallMethod($LoadMethod(foo(PyIR.Global), bar), PYCInt (42))
                  throw n0





            functions:
              f -> f |}]


    let%expect_test _ =
      let source =
        {|
import foo

def f(ok):
    try:
          foo.bar()
    except OverflowError:
        if not ok:
            raise
          |}
      in
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              $ImportName(foo, from_list= [])
              foo(PyIR.Name) <- $ImportName(foo, from_list= [])
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  n0 <- $CallMethod($LoadMethod(foo(PyIR.Global), bar), )
                  jmp b2


                #b1(n6, n5, n4, n3, n2, n1) .except:
                  n7 <- $Compare.exception(n6, OverflowError(PyIR.Global))
                  if n7 then jmp b3(n6, n5, n4, n3, n2, n1) else jmp b4(n6, n5, n4, n3, n2, n1)


                #b3(n13, n12, n11, n10, n9, n8) .label:
                  if $Not(ok(PyIR.Fast)) then jmp b5(n10, n9, n8) else jmp b6(n10, n9, n8)


                #b5(n22, n21, n20) .label:
                  n26 <- GetPreviousException()
                  throw n26


                #b6(n25, n24, n23) .label:
                  jmp b7


                #b7 .label:
                  jmp b2


                #b4(n19, n18, n17, n16, n15, n14) .label:
                  jmp b2


                #b2 .label:
                  return PYCNone





            functions:
              f -> f |}]


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
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  n0 <- $Compare.not_in((a(PyIR.Fast), b(PyIR.Fast)), m(PyIR.Fast))
                  if n0 then jmp b1 else jmp b2


                #b1 .label:
                  n1 <- $Inplace.Subtract(b(PyIR.Fast), PYCInt (1))
                  b(PyIR.Fast) <- n1
                  jmp b0


                #b2 .label:
                  n2 <- $Compare.not_in((a(PyIR.Fast), c(PyIR.Fast)), m(PyIR.Fast))
                  if n2 then jmp b3 else jmp b4


                #b3 .label:
                  n3 <- $Inplace.Add(c(PyIR.Fast), PYCInt (1))
                  c(PyIR.Fast) <- n3
                  jmp b2


                #b4 .label:
                  return PYCNone





            functions:
              f -> f |}]


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
            #b0 .label:
              n0 <- $BuildClass($FuncObj(C, C, {}), PYCString ("C"))
              C(PyIR.Name) <- n0
              f(PyIR.Name) <- $FuncObj(f, f, {(s, PYCString ("zuck")); (y, PYCInt (1)); (z, PYCInt (2)); })
              n1 <- f(PyIR.Name)(PYCInt (0))
              n2 <- f(PyIR.Name)(PYCInt (10), PYCInt (100))
              n3 <- f(PyIR.Name)(PYCInt (100), PYCInt (1000), PYCInt (0))
              n4 <- f(PyIR.Name)(PYCInt (0), PYCInt (0), PYCInt (0), PYCString ("toto"))
              return PYCNone



          objects:
            object C:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("C")
                  return PYCNone




            object f:
              code:
                #b0 .label:
                  return PYCNone





            functions:
              C -> C
              f -> f |}]


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
            #b0 .label:
              n0 <- $BuildClass($FuncObj(TestHook, TestHook, {}), PYCString ("TestHook"))
              TestHook(PyIR.Name) <- n0
              return PYCNone



          objects:
            object TestHook:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("TestHook")
                  __init__(PyIR.Name) <- $FuncObj(__init__, TestHook.__init__, {(exc_type, RuntimeError(PyIR.Name)); (raise_on_events, PYCNone); })
                  return PYCNone



              objects:
                object __init__:
                  code:
                    #b0 .label:
                      return PYCNone





                functions:
                  __init__ -> TestHook.__init__


            functions:
              TestHook -> TestHook |}]


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
            #b0 .label:
              n0 <- $BuildClass($FuncObj(C, C, {}), PYCString ("C"))
              C(PyIR.Name) <- n0
              n1 <- C(PyIR.Name)()
              c(PyIR.Name) <- n1
              n2 <- $CallMethod($LoadMethod(c(PyIR.Name), f), PYCInt (0))
              n3 <- $CallMethod($LoadMethod(c(PyIR.Name), f), PYCInt (0), PYCInt (1))
              n4 <- $CallMethod($LoadMethod(c(PyIR.Name), f), PYCInt (0), PYCInt (1), PYCInt (2))
              return PYCNone



          objects:
            object C:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("C")
                  f(PyIR.Name) <- $FuncObj(f, C.f, {(y, PYCInt (1)); (z, PYCInt (10)); })
                  return PYCNone



              objects:
                object f:
                  code:
                    #b0 .label:
                      n0 <- $Binary.Add(x(PyIR.Fast), y(PyIR.Fast))
                      n1 <- $Binary.Add(n0, z(PyIR.Fast))
                      return n1





                functions:
                  f -> C.f


            functions:
              C -> C |}]


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
            #b0 .label:
              n0 <- $BuildClass($FuncObj(C, C, {}), PYCString ("C"))
              C(PyIR.Name) <- n0
              return PYCNone



          objects:
            object C:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("C")
                  $SETUP_ANNOTATIONS
                  x(PyIR.Name) <- PYCInt (0)
                  __annotations__(PyIR.Name)[PYCString ("x")] <- int(PyIR.Name)
                  return PYCNone





            functions:
              C -> C |}]


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
            #b0 .label:
              $ImportName(dis, from_list= [])
              dis(PyIR.Name) <- $ImportName(dis, from_list= [])
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  n0 <- dis(PyIR.Global).dis(co(PyIR.Fast), file= s(PyIR.Fast))
                  return PYCNone





            functions:
              f -> f |}]


    let%expect_test _ =
      let source =
        {|
def f(name, args):
    return f"foo.{name!r}{name!s}{name!a}"

def test_arguments(x, y, width):
    return f'x={x*y:{width}}'
          |}
      in
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              f(PyIR.Name) <- $FuncObj(f, f, {})
              test_arguments(PyIR.Name) <- $FuncObj(test_arguments, test_arguments, {})
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  n0 <- $FormatFn.repr(name(PyIR.Fast))
                  n1 <- $Format(n0, PYCNone)
                  n2 <- $FormatFn.str(name(PyIR.Fast))
                  n3 <- $Format(n2, PYCNone)
                  n4 <- $FormatFn.ascii(name(PyIR.Fast))
                  n5 <- $Format(n4, PYCNone)
                  return $Concat(PYCString ("foo."), n1, n3, n5)




            object test_arguments:
              code:
                #b0 .label:
                  n0 <- $Binary.Multiply(x(PyIR.Fast), y(PyIR.Fast))
                  n1 <- $Format(width(PyIR.Fast), PYCNone)
                  n2 <- $Format(n0, n1)
                  return $Concat(PYCString ("x="), n2)





            functions:
              f -> f
              test_arguments -> test_arguments |}]


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
            #b0 .label:
              $ImportName(decimal, from_list= [])
              decimal(PyIR.Name) <- $ImportName(decimal, from_list= [])
              assertEqual(PyIR.Name) <- $FuncObj(assertEqual, assertEqual, {})
              test_format_specifier_expressions(PyIR.Name) <- $FuncObj(test_format_specifier_expressions, test_format_specifier_expressions, {})
              return PYCNone



          objects:
            object assertEqual:
              code:
                #b0 .label:
                  return PYCNone




            object test_format_specifier_expressions:
              code:
                #b0 .label:
                  width(PyIR.Fast) <- PYCInt (10)
                  precision(PyIR.Fast) <- PYCInt (4)
                  n0 <- $CallMethod($LoadMethod(decimal(PyIR.Global), Decimal), PYCString ("12.34567"))
                  value(PyIR.Fast) <- n0
                  n1 <- $Format(width(PyIR.Fast), PYCNone)
                  n2 <- $Format(precision(PyIR.Fast), PYCNone)
                  n3 <- $Format(value(PyIR.Fast), $Concat(n1, PYCString ("."), n2))
                  n4 <- assertEqual(PyIR.Global)($Concat(PYCString ("result: "), n3))
                  return PYCNone





            functions:
              assertEqual -> assertEqual
              test_format_specifier_expressions -> test_format_specifier_expressions |}]


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
            #b0 .label:
              pos(PyIR.Name) <- $FuncObj(pos, pos, {})
              neg(PyIR.Name) <- $FuncObj(neg, neg, {})
              test_not(PyIR.Name) <- $FuncObj(test_not, test_not, {})
              inv(PyIR.Name) <- $FuncObj(inv, inv, {})
              return PYCNone



          objects:
            object pos:
              code:
                #b0 .label:
                  n0 <- $Unary.Positive(x(PyIR.Fast))
                  return n0




            object neg:
              code:
                #b0 .label:
                  n0 <- $Unary.Negative(x(PyIR.Fast))
                  return n0




            object test_not:
              code:
                #b0 .label:
                  n0 <- $Unary.Not(x(PyIR.Fast))
                  return n0




            object inv:
              code:
                #b0 .label:
                  n0 <- $Unary.Invert(x(PyIR.Fast))
                  return n0





            functions:
              inv -> inv
              neg -> neg
              pos -> pos
              test_not -> test_not |}]


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
            #b0 .label:
              $SETUP_ANNOTATIONS
              __annotations__(PyIR.Name)[PYCString ("x")] <- int(PyIR.Name)
              x(PyIR.Name) <- PYCInt (0)
              y(PyIR.Name) <- PYCString ("zuck")
              __annotations__(PyIR.Name)[PYCString ("y")] <- str(PyIR.Name)
              $ImportName(C, from_list= [])
              C(PyIR.Name) <- $ImportName(C, from_list= [])
              z(PyIR.Name) <- PYCInt (42)
              __annotations__(PyIR.Name)[PYCString ("z")] <- C(PyIR.Name).T
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  u(PyIR.Fast) <- PYCInt (0)
                  v(PyIR.Fast) <- PYCString ("tata")
                  return PYCNone





            functions:
              f -> f |}]


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
            #b0 .label:
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  n0 <- $GetIter(match(PyIR.Fast))
                  jmp b1(n0)


                #b1(n1) .label:
                  n2 <- $NextIter(n1)
                  n3 <- $HasNextIter(n2)
                  if n3 then jmp b2 else jmp b3


                #b2 .label:
                  n4 <- $IterData(n2)
                  item(PyIR.Fast) <- n4
                  n5 <- $Compare.eq(it(PyIR.Fast)[n(PyIR.Fast)], item(PyIR.Fast))
                  if $Not(n5) then jmp b4(n1) else jmp b5(n1)


                #b4(n6) .label:
                  throw AssertionError(PyIR.Global)


                #b5(n7) .label:
                  n8 <- $Inplace.Add(n(PyIR.Fast), PYCInt (1))
                  n(PyIR.Fast) <- n8
                  jmp b1(n7)


                #b3 .label:
                  return PYCNone





            functions:
              f -> f |}]


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
            #b0 .label:
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  n0 <- $GetIter(foo(PyIR.Fast))
                  jmp b1(n0)


                #b1(n1) .label:
                  n2 <- $NextIter(n1)
                  n3 <- $HasNextIter(n2)
                  if n3 then jmp b2 else jmp b3


                #b2 .label:
                  n4 <- $IterData(n2)
                  path(PyIR.Fast) <- n4
                  if path(PyIR.Fast) then jmp b4(n1) else jmp b1(n1)


                #b4(n5) .label:
                  return PYCNone


                #b3 .label:
                  return PYCNone





            functions:
              f -> f |}]


    let%expect_test _ =
      let source =
        {|
try:
  print("TRY BLOCK")
except:
  print("EXCEPT BLOCK")
print("END")
          |}
      in
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              n0 <- print(PyIR.Name)(PYCString ("TRY BLOCK"))
              jmp b2


            #b1(n6, n5, n4, n3, n2, n1) .except:
              n7 <- print(PyIR.Name)(PYCString ("EXCEPT BLOCK"))
              jmp b3


            #b3 .label:
              jmp b2


            #b2 .label:
              n8 <- print(PyIR.Name)(PYCString ("END"))
              return PYCNone |}]


    let%expect_test _ =
      let source =
        {|
import os


try:
    page_size = os.sysconf('SC_PAGESIZE')
except (ValueError, AttributeError):
    try:
        page_size = 0
    except (ValueError, AttributeError):
        page_size = 4096
                 |}
      in
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              $ImportName(os, from_list= [])
              os(PyIR.Name) <- $ImportName(os, from_list= [])
              n0 <- $CallMethod($LoadMethod(os(PyIR.Name), sysconf), PYCString ("SC_PAGESIZE"))
              page_size(PyIR.Name) <- n0
              jmp b2


            #b1(n6, n5, n4, n3, n2, n1) .except:
              n7 <- $Compare.exception(n6, (ValueError(PyIR.Name), AttributeError(PyIR.Name)))
              if n7 then jmp b3(n6, n5, n4, n3, n2, n1) else jmp b4(n6, n5, n4, n3, n2, n1)


            #b3(n13, n12, n11, n10, n9, n8) .label:
              page_size(PyIR.Name) <- PYCInt (0)
              jmp b6(n10, n9, n8)


            #b5(n31, n30, n29, n28, n27, n26, n22, n21, n20) .except:
              n32 <- $Compare.exception(n31, (ValueError(PyIR.Name), AttributeError(PyIR.Name)))
              if n32 then jmp b7(n31, n30, n29, n28, n27, n26, n22, n21, n20) else
              jmp b8(n31, n30, n29, n28, n27, n26, n22, n21, n20)


            #b7(n41, n40, n39, n38, n37, n36, n35, n34, n33) .label:
              page_size(PyIR.Name) <- PYCInt (4096)
              jmp b9(n35, n34, n33)


            #b9(n53, n52, n51) .label:
              jmp b6(n53, n52, n51)


            #b8(n50, n49, n48, n47, n46, n45, n44, n43, n42) .label:
              jmp b6(n44, n43, n42)


            #b6(n25, n24, n23) .label:
              jmp b10


            #b10 .label:
              jmp b2


            #b4(n19, n18, n17, n16, n15, n14) .label:
              jmp b2


            #b2 .label:
              return PYCNone |}]


    let%expect_test _ =
      let source =
        {|
import foo

def f(x):
    for i in x:
        e = foo.Foo()
        try:
            print("yolo")
        finally:
            e.bar()
        |}
      in
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              $ImportName(foo, from_list= [])
              foo(PyIR.Name) <- $ImportName(foo, from_list= [])
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  n0 <- $GetIter(x(PyIR.Fast))
                  jmp b1(n0)


                #b1(n1) .label:
                  n2 <- $NextIter(n1)
                  n3 <- $HasNextIter(n2)
                  if n3 then jmp b2 else jmp b3


                #b2 .label:
                  n4 <- $IterData(n2)
                  i(PyIR.Fast) <- n4
                  n5 <- $CallMethod($LoadMethod(foo(PyIR.Global), Foo), )
                  e(PyIR.Fast) <- n5
                  n7 <- print(PyIR.Global)(PYCString ("yolo"))
                  jmp b4(n1)


                #b4(n6) .finally:
                  n9 <- $CallMethod($LoadMethod(e(PyIR.Fast), bar), )
                  jmp b1(n6)


                #b3 .label:
                  return PYCNone





            functions:
              f -> f |}]


    let%expect_test _ =
      let source =
        {|
from foo import ERROR

with open("foo", "r") as fp:
    for line in fp:
        try:
            print("TRY")
        except ERROR:
            print("EXCEPT")
        else:
            print("ELSE")
        |}
      in
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              $ImportName(foo, from_list= [ERROR])
              ERROR(PyIR.Name) <- $ImportFrom($ImportName(foo, from_list= [ERROR]), name= ERROR)
              n0 <- open(PyIR.Name)(PYCString ("foo"), PYCString ("r"))
              n1 <- $LoadMethod(n0, __enter__)()
              fp(PyIR.Name) <- n1
              n3 <- $GetIter(fp(PyIR.Name))
              jmp b2(n3, CM(n0).__exit__)


            #b2(n5, n4) .label:
              n6 <- $NextIter(n5)
              n7 <- $HasNextIter(n6)
              if n7 then jmp b3(n4) else jmp b4(n4)


            #b3(n8) .label:
              n10 <- $IterData(n6)
              line(PyIR.Name) <- n10
              n13 <- print(PyIR.Name)(PYCString ("TRY"))
              jmp b6(n5, n8)


            #b5(n21, n20, n19, n18, n17, n16, n12, n11) .except:
              n22 <- $Compare.exception(n21, ERROR(PyIR.Name))
              if n22 then jmp b7(n21, n20, n19, n18, n17, n16, n12, n11) else
              jmp b8(n21, n20, n19, n18, n17, n16, n12, n11)


            #b7(n30, n29, n28, n27, n26, n25, n24, n23) .label:
              n39 <- print(PyIR.Name)(PYCString ("EXCEPT"))
              jmp b9(n24, n23)


            #b9(n41, n40) .label:
              jmp b2(n41, n40)


            #b8(n38, n37, n36, n35, n34, n33, n32, n31) .label:
              jmp b6(n32, n31)


            #b6(n15, n14) .label:
              n44 <- print(PyIR.Name)(PYCString ("ELSE"))
              jmp b2(n15, n14)


            #b4(n9) .label:
              jmp b1(n9)


            #b1(n2) .finally:
              n48 <- n2(PYCNone, PYCNone, PYCNone)
              return PYCNone |}]


    let%expect_test _ =
      let source =
        {|
TICKS=0

def subhelper():
    global TICKS
    TICKS += 2
    for i in range(2):
        try:
            print("foo")
        except AttributeError:
            TICKS += 3
        |}
      in
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              TICKS(PyIR.Global) <- PYCInt (0)
              subhelper(PyIR.Name) <- $FuncObj(subhelper, subhelper, {})
              return PYCNone



          objects:
            object subhelper:
              code:
                #b0 .label:
                  n0 <- $Inplace.Add(TICKS(PyIR.Global), PYCInt (2))
                  TICKS(PyIR.Global) <- n0
                  n1 <- range(PyIR.Global)(PYCInt (2))
                  n2 <- $GetIter(n1)
                  jmp b1(n2)


                #b1(n3) .label:
                  n4 <- $NextIter(n3)
                  n5 <- $HasNextIter(n4)
                  if n5 then jmp b2 else jmp b3


                #b2 .label:
                  n6 <- $IterData(n4)
                  i(PyIR.Fast) <- n6
                  n8 <- print(PyIR.Global)(PYCString ("foo"))
                  jmp b1(n3)


                #b4(n15, n14, n13, n12, n11, n10, n7) .except:
                  n16 <- $Compare.exception(n15, AttributeError(PyIR.Global))
                  if n16 then jmp b5(n15, n14, n13, n12, n11, n10, n7) else jmp b6(
                                                                            n15, n14, n13, n12, n11, n10, n7)


                #b5(n23, n22, n21, n20, n19, n18, n17) .label:
                  n31 <- $Inplace.Add(TICKS(PyIR.Global), PYCInt (3))
                  TICKS(PyIR.Global) <- n31
                  jmp b7(n17)


                #b7(n32) .label:
                  jmp b1(n32)


                #b6(n30, n29, n28, n27, n26, n25, n24) .label:
                  jmp b1(n24)


                #b3 .label:
                  return PYCNone





            functions:
              subhelper -> subhelper |}]


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
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              n0 <- $BuildClass($FuncObj(defaultdict, defaultdict, {}), PYCString ("defaultdict"))
              defaultdict(PyIR.Name) <- n0
              return PYCNone



          objects:
            object defaultdict:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("defaultdict")
                  __getitem__(PyIR.Name) <- $FuncObj(__getitem__, defaultdict.__getitem__, {})
                  return PYCNone



              objects:
                object __getitem__:
                  code:
                    #b0 .label:
                      return PYCInt (42)


                    #b1(n5, n4, n3, n2, n1, n0) .except:
                      n6 <- $Compare.exception(n5, KeyError(PyIR.Global))
                      if n6 then jmp b2(n5, n4, n3, n2, n1, n0) else jmp b3(n5, n4, n3, n2, n1, n0)


                    #b2(n12, n11, n10, n9, n8, n7) .label:
                      jmp b4(self(PyIR.Fast).default)


                    #b4(n19) .label:
                      return n19


                    #b3(n18, n17, n16, n15, n14, n13) .label:
                      return PYCNone





                functions:
                  __getitem__ -> defaultdict.__getitem__


            functions:
              defaultdict -> defaultdict |}]


    let%expect_test _ =
      let source =
        {|
def foo():
          pass

try:
          foo()
except C as c:
          print(c)
          |}
      in
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              foo(PyIR.Name) <- $FuncObj(foo, foo, {})
              n0 <- foo(PyIR.Name)()
              jmp b2


            #b1(n6, n5, n4, n3, n2, n1) .except:
              n7 <- $Compare.exception(n6, C(PyIR.Name))
              if n7 then jmp b3(n6, n5, n4, n3, n2, n1) else jmp b4(n6, n5, n4, n3, n2, n1)


            #b3(n13, n12, n11, n10, n9, n8) .label:
              c(PyIR.Name) <- n12
              n23 <- print(PyIR.Name)(c(PyIR.Name))
              jmp b5(n10, n9, n8)


            #b5(n22, n21, n20) .finally:
              c(PyIR.Name) <- PYCNone
              n27 <- $DeletePyIR.Name(c)()
              jmp b6


            #b6 .label:
              jmp b2


            #b4(n19, n18, n17, n16, n15, n14) .label:
              jmp b2


            #b2 .label:
              return PYCNone



          objects:
            object foo:
              code:
                #b0 .label:
                  return PYCNone





            functions:
              foo -> foo |}]


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
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              gx(PyIR.Global) <- PYCInt (100)
              f(PyIR.Name) <- $FuncObj(f, f, {})
              n0 <- f(PyIR.Name)(PYCInt (42))
              g(PyIR.Name) <- n0
              n1 <- g(PyIR.Name)()
              n2 <- print(PyIR.Name)(n1)
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  lx(PyIR.Deref) <- PYCInt (1000)
                  inner(PyIR.Fast) <- $FuncObj(inner, f.<locals>.inner, {})
                  lx(PyIR.Deref) <- PYCInt (1664)
                  return inner(PyIR.Fast)



              objects:
                object inner:
                  code:
                    #b0 .label:
                      ix(PyIR.Fast) <- PYCInt (20)
                      n0 <- print(PyIR.Global)(gx(PyIR.Global))
                      n1 <- print(PyIR.Global)(ax(PyIR.Deref))
                      n2 <- print(PyIR.Global)(lx(PyIR.Deref))
                      n3 <- print(PyIR.Global)(ix(PyIR.Fast))
                      gx(PyIR.Global) <- PYCInt (10)
                      lx(PyIR.Deref) <- PYCInt (2)
                      return lx(PyIR.Deref)





                functions:
                  inner -> f.<locals>.inner


            functions:
              f -> f |}]


    let%expect_test _ =
      let source =
        {|
values = [1, 2, [3, 4] , 5]
values2 = ('a', 'b')

result = (*[10, 100], *values, *values2)

print(result) # (10, 100, 1, 2, [3, 4], 5, 'a', 'b')

result = [*values, *values2] # [10, 100, 1, 2, [3, 4], 5, 'a', 'b']
print(result)
        |}
      in
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              values(PyIR.Name) <- [PYCInt (1), PYCInt (2), [PYCInt (3), PYCInt (4)], PYCInt (5)]
              values2(PyIR.Name) <- PYCTuple ([|PYCString ("a"); PYCString ("b")|])
              result(PyIR.Name) <- (packed)($Packed([PYCInt (10), PYCInt (100)]), $Packed(values(PyIR.Name)), $Packed(values2(PyIR.Name)))
              n0 <- print(PyIR.Name)(result(PyIR.Name))
              result(PyIR.Name) <- (packed)[$Packed(values(PyIR.Name)), $Packed(values2(PyIR.Name))]
              n1 <- print(PyIR.Name)(result(PyIR.Name))
              return PYCNone |}]


    let%expect_test _ =
      let source =
        {|
def f(dummy, dummy2, dummy3, dummy4, **dummyA):
    print("dummy = ", dummy)
    print("dummy2= ", dummy2)
    print("dummy3= ", dummy3)
    print("dummy4= ", dummy4)
    for (k, v) in dummyA.items():
        print("{} = {}".format(k, v))

def g(dummy, dummy2, dummy3, dummy4):
    print("dummy = ", dummy)
    print("dummy2= ", dummy2)
    print("dummy3= ", dummy3)
    print("dummy4= ", dummy4)

def start():
    x = (3, 4)
    f(*(1, 2), *x, **{"test": 42})
    f(*(1, 2), 'a', 'b', **{"test": 42})
    g(*(1, 2), *x)

start()
        |}
      in
      test source ;
      [%expect
        {xxx|
        module
        object dummy:
          code:
            #b0 .label:
              f(PyIR.Name) <- $FuncObj(f, f, {})
              g(PyIR.Name) <- $FuncObj(g, g, {})
              start(PyIR.Name) <- $FuncObj(start, start, {})
              n0 <- start(PyIR.Name)()
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  n0 <- print(PyIR.Global)(PYCString ("dummy = "), dummy(PyIR.Fast))
                  n1 <- print(PyIR.Global)(PYCString ("dummy2= "), dummy2(PyIR.Fast))
                  n2 <- print(PyIR.Global)(PYCString ("dummy3= "), dummy3(PyIR.Fast))
                  n3 <- print(PyIR.Global)(PYCString ("dummy4= "), dummy4(PyIR.Fast))
                  n4 <- $CallMethod($LoadMethod(dummyA(PyIR.Fast), items), )
                  n5 <- $GetIter(n4)
                  jmp b1(n5)


                #b1(n6) .label:
                  n7 <- $NextIter(n6)
                  n8 <- $HasNextIter(n7)
                  if n8 then jmp b2 else jmp b3


                #b2 .label:
                  n9 <- $IterData(n7)
                  k(PyIR.Fast) <- n9[PYCInt (0)]
                  v(PyIR.Fast) <- n9[PYCInt (1)]
                  n10 <- $CallMethod($LoadMethod(PYCString ("{} = {}"), format),
                    k(PyIR.Fast), v(PyIR.Fast))
                  n11 <- print(PyIR.Global)(n10)
                  jmp b1(n6)


                #b3 .label:
                  return PYCNone




            object g:
              code:
                #b0 .label:
                  n0 <- print(PyIR.Global)(PYCString ("dummy = "), dummy(PyIR.Fast))
                  n1 <- print(PyIR.Global)(PYCString ("dummy2= "), dummy2(PyIR.Fast))
                  n2 <- print(PyIR.Global)(PYCString ("dummy3= "), dummy3(PyIR.Fast))
                  n3 <- print(PyIR.Global)(PYCString ("dummy4= "), dummy4(PyIR.Fast))
                  return PYCNone




            object start:
              code:
                #b0 .label:
                  x(PyIR.Fast) <- PYCTuple ([|PYCInt (3); PYCInt (4)|])
                  n0 <- f(PyIR.Global)($Packed((packed)($Packed(PYCTuple ([|PYCInt (1); PYCInt (2)|])), $Packed(x(PyIR.Fast)))), $PackedMap({|
                                       PYCString ("test"), PYCInt (42)|})) !packed
                  n1 <- f(PyIR.Global)($Packed((packed)($Packed(PYCTuple ([|PYCInt (1); PYCInt (2)|])), $Packed(
                                                        PYCTuple ([|PYCString ("a"); PYCString ("b")|])))), $PackedMap({|
                                       PYCString ("test"), PYCInt (42)|})) !packed
                  n2 <- g(PyIR.Global)($Packed((packed)($Packed(PYCTuple ([|PYCInt (1); PYCInt (2)|])), $Packed(x(PyIR.Fast))))) !packed
                  return PYCNone





            functions:
              f -> f
              g -> g
              start -> start |xxx}]


    let%expect_test _ =
      let source =
        {|
def f(foo, a, b, c):
    foo.f(a)

    foo.f(*b)
    foo.f(a, *b)

    foo.f(**c)
    foo.f(*b, **c)
    foo.f(a, **c)
    foo.f(a, *b, **c)
        |}
      in
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  n0 <- $CallMethod($LoadMethod(foo(PyIR.Fast), f), a(PyIR.Fast))
                  n1 <- foo(PyIR.Fast).f($Packed(b(PyIR.Fast))) !packed
                  n2 <- foo(PyIR.Fast).f($Packed((packed)($Packed((a(PyIR.Fast))), $Packed(b(PyIR.Fast))))) !packed
                  n3 <- foo(PyIR.Fast).f($Packed(()), $PackedMap(c(PyIR.Fast))) !packed
                  n4 <- foo(PyIR.Fast).f($Packed(b(PyIR.Fast)), $PackedMap(c(PyIR.Fast))) !packed
                  n5 <- foo(PyIR.Fast).f($Packed((a(PyIR.Fast))), $PackedMap(c(PyIR.Fast))) !packed
                  n6 <- foo(PyIR.Fast).f($Packed((packed)($Packed((a(PyIR.Fast))), $Packed(b(PyIR.Fast)))), $PackedMap(c(PyIR.Fast))) !packed
                  return PYCNone





            functions:
              f -> f |}]


    let%expect_test _ =
      let source =
        {|
d0 = {0: 0, 1:1}
d1 = {'a': 0, 'b': 1}
x = {**d0, **d1}
print(x)

def f(x, **kwargs):
    print(x)
    for (k, v) in kwargs.items():
        print(k, v)

d1 = {'a': 0, 'b': 1}
f(**d1, x=42)
          |}
      in
      test source ;
      [%expect
        {xxx|
        module
        object dummy:
          code:
            #b0 .label:
              d0(PyIR.Name) <- {PYCInt (0): PYCInt (0), PYCInt (1): PYCInt (1), }
              d1(PyIR.Name) <- {PYCString ("a"): PYCInt (0), PYCString ("b"): PYCInt (1), }
              x(PyIR.Name) <- (packed){|$Packed(d0(PyIR.Name)), $Packed(d1(PyIR.Name))|}
              n0 <- print(PyIR.Name)(x(PyIR.Name))
              f(PyIR.Name) <- $FuncObj(f, f, {})
              d1(PyIR.Name) <- {PYCString ("a"): PYCInt (0), PYCString ("b"): PYCInt (1), }
              n1 <- f(PyIR.Name)($Packed(()), $PackedMap((packed){|$Packed(d1(PyIR.Name)), $Packed({|
                                                                   PYCString ("x"),
                                                                   PYCInt (42)|})|})) !packed
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  n0 <- print(PyIR.Global)(x(PyIR.Fast))
                  n1 <- $CallMethod($LoadMethod(kwargs(PyIR.Fast), items), )
                  n2 <- $GetIter(n1)
                  jmp b1(n2)


                #b1(n3) .label:
                  n4 <- $NextIter(n3)
                  n5 <- $HasNextIter(n4)
                  if n5 then jmp b2 else jmp b3


                #b2 .label:
                  n6 <- $IterData(n4)
                  k(PyIR.Fast) <- n6[PYCInt (0)]
                  v(PyIR.Fast) <- n6[PYCInt (1)]
                  n7 <- print(PyIR.Global)(k(PyIR.Fast), v(PyIR.Fast))
                  jmp b1(n3)


                #b3 .label:
                  return PYCNone





            functions:
              f -> f |xxx}]


    let%expect_test _ =
      let source =
        {|
def f():
          return range(10)

(a, b, *lst, x, y, z) = f()
print(lst) # [2, 3, 4, 5, 6]
          |}
      in
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              f(PyIR.Name) <- $FuncObj(f, f, {})
              n0 <- f(PyIR.Name)()
              n1 <- $UnpackEx(PYCInt (2), PYCInt (3), n0)
              a(PyIR.Name) <- n1[PYCInt (0)]
              b(PyIR.Name) <- n1[PYCInt (1)]
              lst(PyIR.Name) <- n1[PYCInt (2)]
              x(PyIR.Name) <- n1[PYCInt (3)]
              y(PyIR.Name) <- n1[PYCInt (4)]
              z(PyIR.Name) <- n1[PYCInt (5)]
              n2 <- print(PyIR.Name)(lst(PyIR.Name))
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  n0 <- range(PyIR.Global)(PYCInt (10))
                  return n0





            functions:
              f -> f |}]


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
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              $ImportName(itertools, from_list= [])
              itertools(PyIR.Name) <- $ImportName(itertools, from_list= [])
              f(PyIR.Name) <- $FuncObj(f, f, {})
              n0 <- $BuildClass($FuncObj(AsyncYieldFrom, AsyncYieldFrom, {}), PYCString ("AsyncYieldFrom"))
              AsyncYieldFrom(PyIR.Name) <- n0
              powerset(PyIR.Name) <- $FuncObj(powerset, powerset, {})
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  n0 <- $Yield(PYCInt (42))
                  return PYCNone




            object AsyncYieldFrom:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("AsyncYieldFrom")
                  __await__(PyIR.Name) <- $FuncObj(__await__, AsyncYieldFrom.__await__, {})
                  return PYCNone



              objects:
                object __await__:
                  code:
                    #b0 .label:
                      n0 <- $GetYieldFromIter(self(PyIR.Fast).obj)
                      n1 <- $YieldFrom(n0, PYCNone)
                      return PYCNone





                functions:
                  __await__ -> AsyncYieldFrom.__await__

              object powerset:
                code:
                  #b0 .label:
                    n0 <- len(PyIR.Global)(s(PyIR.Fast))
                    n1 <- $Binary.Add(n0, PYCInt (1))
                    n2 <- range(PyIR.Global)(n1)
                    n3 <- $GetIter(n2)
                    jmp b1(n3)


                  #b1(n4) .label:
                    n5 <- $NextIter(n4)
                    n6 <- $HasNextIter(n5)
                    if n6 then jmp b2 else jmp b3


                  #b2 .label:
                    n7 <- $IterData(n5)
                    i(PyIR.Fast) <- n7
                    n8 <- $CallMethod($LoadMethod(itertools(PyIR.Global), combinations),
                      s(PyIR.Fast), i(PyIR.Fast))
                    n9 <- map(PyIR.Global)(frozenset(PyIR.Global), n8)
                    n10 <- $GetYieldFromIter(n9)
                    n11 <- $YieldFrom(n10, PYCNone)
                    jmp b1(n4)


                  #b3 .label:
                    return PYCNone





              functions:
                AsyncYieldFrom -> AsyncYieldFrom
                f -> f
                powerset -> powerset |}]


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
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              n0 <- $GetIter(l(PyIR.Name))
              n1 <- $FuncObj(<listcomp>, <listcomp>, {})(n0)
              g(PyIR.Name) <- n1
              n2 <- $GetIter(l(PyIR.Name))
              n3 <- $FuncObj(<listcomp>, <listcomp>, {})(n2)
              g0(PyIR.Name) <- n3
              n4 <- print(PyIR.Name)(g(PyIR.Name))
              n5 <- print(PyIR.Name)(g0(PyIR.Name))
              f(PyIR.Name) <- $FuncObj(f, f, {})
              return PYCNone



          objects:
            object <listcomp>:
              code:
                #b0 .label:
                  jmp b1(.0(PyIR.Fast), [])


                #b1(n1, n0) .label:
                  n2 <- $NextIter(n1)
                  n3 <- $HasNextIter(n2)
                  if n3 then jmp b2(n0) else jmp b3(n0)


                #b2(n4) .label:
                  n6 <- $IterData(n2)
                  x(PyIR.Fast) <- n6
                  n7 <- $Binary.Add(x(PyIR.Fast), PYCInt (1))
                  n8 <- $ListAppend(n4, n7)
                  jmp b1(n1, n4)


                #b3(n5) .label:
                  return n5




            object <listcomp>:
              code:
                #b0 .label:
                  jmp b1(.0(PyIR.Fast), [])


                #b1(n1, n0) .label:
                  n2 <- $NextIter(n1)
                  n3 <- $HasNextIter(n2)
                  if n3 then jmp b2(n0) else jmp b3(n0)


                #b2(n4) .label:
                  n6 <- $IterData(n2)
                  x(PyIR.Fast) <- n6
                  n7 <- $Binary.Add(x(PyIR.Fast), PYCInt (2))
                  n8 <- $ListAppend(n4, n7)
                  jmp b1(n1, n4)


                #b3(n5) .label:
                  return n5




            object f:
              code:
                #b0 .label:
                  n0 <- $GetIter(l(PyIR.Fast))
                  n1 <- $FuncObj(<listcomp>, f.<locals>.<listcomp>, {})(n0)
                  r(PyIR.Fast) <- n1
                  n2 <- $GetIter(l(PyIR.Fast))
                  n3 <- $FuncObj(<listcomp>, f.<locals>.<listcomp>, {})(n2)
                  r0(PyIR.Fast) <- n3
                  n4 <- print(PyIR.Global)(r(PyIR.Fast))
                  n5 <- print(PyIR.Global)(r0(PyIR.Fast))
                  return PYCNone



              objects:
                object <listcomp>:
                  code:
                    #b0 .label:
                      jmp b1(.0(PyIR.Fast), [])


                    #b1(n1, n0) .label:
                      n2 <- $NextIter(n1)
                      n3 <- $HasNextIter(n2)
                      if n3 then jmp b2(n0) else jmp b3(n0)


                    #b2(n4) .label:
                      n6 <- $IterData(n2)
                      x(PyIR.Fast) <- n6
                      n7 <- $Binary.Add(x(PyIR.Fast), PYCInt (1))
                      n8 <- $ListAppend(n4, n7)
                      jmp b1(n1, n4)


                    #b3(n5) .label:
                      return n5




                object <listcomp>:
                  code:
                    #b0 .label:
                      jmp b1(.0(PyIR.Fast), [])


                    #b1(n1, n0) .label:
                      n2 <- $NextIter(n1)
                      n3 <- $HasNextIter(n2)
                      if n3 then jmp b2(n0) else jmp b3(n0)


                    #b2(n4) .label:
                      n6 <- $IterData(n2)
                      x(PyIR.Fast) <- n6
                      n7 <- $Binary.Add(x(PyIR.Fast), PYCInt (2))
                      n8 <- $ListAppend(n4, n7)
                      jmp b1(n1, n4)


                    #b3(n5) .label:
                      return n5





                functions:
                  <listcomp> -> f.<locals>.<listcomp>


              functions:
                <listcomp> -> <listcomp>
                f -> f |}]


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
      test source ;
      [%expect
        {xxx|
        module
        object dummy:
          code:
            #b0 .label:
              f(PyIR.Name) <- $FuncObj(f, f, {})
              g(PyIR.Name) <- $FuncObj(g, g, {})
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  n0 <- $GetIter(l(PyIR.Fast))
                  n1 <- $FuncObj(<setcomp>, f.<locals>.<setcomp>, {})(n0)
                  r(PyIR.Fast) <- n1
                  return r(PyIR.Fast)



              objects:
                object <setcomp>:
                  code:
                    #b0 .label:
                      jmp b1(.0(PyIR.Fast), {})


                    #b1(n1, n0) .label:
                      n2 <- $NextIter(n1)
                      n3 <- $HasNextIter(n2)
                      if n3 then jmp b2(n0) else jmp b3(n0)


                    #b2(n4) .label:
                      n6 <- $IterData(n2)
                      x(PyIR.Fast) <- n6
                      n7 <- $Binary.Add(x(PyIR.Fast), PYCInt (1))
                      n8 <- $SetAdd(n4, n7)
                      jmp b1(n1, n4)


                    #b3(n5) .label:
                      return n5





                functions:
                  <setcomp> -> f.<locals>.<setcomp>

              object g:
                code:
                  #b0 .label:
                    n0 <- $GetIter(l(PyIR.Fast))
                    n1 <- $FuncObj(<dictcomp>, g.<locals>.<dictcomp>, {})(n0)
                    squared_dict(PyIR.Fast) <- n1
                    return r(PyIR.Global)



                objects:
                  object <dictcomp>:
                    code:
                      #b0 .label:
                        jmp b1(.0(PyIR.Fast), {||})


                      #b1(n1, n0) .label:
                        n2 <- $NextIter(n1)
                        n3 <- $HasNextIter(n2)
                        if n3 then jmp b2(n0) else jmp b3(n0)


                      #b2(n4) .label:
                        n6 <- $IterData(n2)
                        num(PyIR.Fast) <- n6
                        n7 <- $Binary.Power(num(PyIR.Fast), PYCInt (2))
                        n8 <- $DictSetItem(n4, num(PyIR.Fast), n7)
                        jmp b1(n1, n4)


                      #b3(n5) .label:
                        return n5





                  functions:
                    <dictcomp> -> g.<locals>.<dictcomp>


              functions:
                f -> f
                g -> g |xxx}]


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
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              n0 <- $BuildClass($FuncObj(C, C, {}), PYCString ("C"))
              C(PyIR.Name) <- n0
              n1 <- C(PyIR.Name)()
              c(PyIR.Name) <- n1
              n2 <- $DeletePyIR.Name(c)()
              n3 <- C(PyIR.Name)()
              c0(PyIR.Global) <- n3
              n4 <- $DeleteAttr(foo)(c0(PyIR.Global))
              f(PyIR.Name) <- $FuncObj(f, f, {})
              g(PyIR.Name) <- $FuncObj(g, g, {})
              return PYCNone



          objects:
            object C:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("C")
                  return PYCNone




            object f:
              code:
                #b0 .label:
                  n0 <- $DeletePyIR.Global(c0)()
                  n1 <- $DeletePyIR.Fast(x)()
                  z(PyIR.Deref) <- PYCInt (0)
                  inner(PyIR.Fast) <- $FuncObj(inner, f.<locals>.inner, {})
                  return PYCNone



              objects:
                object inner:
                  code:
                    #b0 .label:
                      n0 <- $DeletePyIR.Deref(z)()
                      return PYCNone





                functions:
                  inner -> f.<locals>.inner

              object g:
                code:
                  #b0 .label:
                    n0 <- $DeleteSubscr(a(PyIR.Fast), b(PyIR.Fast))
                    return PYCNone





              functions:
                C -> C
                f -> f
                g -> g |}]


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
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              f(PyIR.Name) <- $FuncObj(f, f, {})
              g(PyIR.Name) <- $FuncObj(g, g, {})
              return PYCNone



          objects:
            object f:
              code:
                #b0 .label:
                  return PYCBool (true)




            object g:
              code:
                #b0 .label:
                  n0 <- f(PyIR.Global)()
                  n1 <- $GetAwaitable(n0)
                  n2 <- $YieldFrom(n1, PYCNone)
                  if n1 then jmp b1 else jmp b2


                #b1 .label:
                  n3 <- print(PyIR.Global)(PYCInt (0))
                  jmp b3


                #b2 .label:
                  n4 <- print(PyIR.Global)(PYCInt (1))
                  jmp b3


                #b3 .label:
                  return PYCNone





            functions:
              f -> f
              g -> g |}]


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
      test source ;
      [%expect
        {|
        module
        object dummy:
          code:
            #b0 .label:
              n0 <- $BuildClass($FuncObj(C, C, {}), PYCString ("C"))
              C(PyIR.Name) <- n0
              return PYCNone



          objects:
            object C:
              code:
                #b0 .label:
                  __module__(PyIR.Name) <- __name__(PyIR.Name)
                  __qualname__(PyIR.Name) <- PYCString ("C")
                  f(PyIR.Name) <- $FuncObj(f, C.f, {})
                  return PYCNone



              objects:
                object f:
                  code:
                    #b0 .label:
                      $ImportName(binascii, from_list= [])
                      binascii(PyIR.Deref) <- $ImportName(binascii, from_list= [])
                      n0 <- $BuildClass($FuncObj(D, D, {}), PYCString ("D"))
                      D(PyIR.Fast) <- n0
                      return PYCNone



                  objects:
                    object D:
                      code:
                        #b0 .label:
                          __module__(PyIR.Name) <- __name__(PyIR.Name)
                          __qualname__(PyIR.Name) <- PYCString ("C.f.<locals>.D")
                          g(PyIR.Name) <- $FuncObj(g, C.f.<locals>.D.g, {(unhexlify, binascii(PyIR.Deref).unhexlify); })
                          return PYCNone



                      objects:
                        object g:
                          code:
                            #b0 .label:
                              return PYCNone





                        functions:
                          g -> C.f.<locals>.D.g


                    functions:
                      D -> D


                  functions:
                    f -> C.f


                functions:
                  C -> C |}]
  end )
