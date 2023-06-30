(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let filename = "dummy.py"

let sourcefile = Textual.SourceFile.create filename

let test ?(typecheck = true) source =
  Py.initialize ~interpreter:Version.python_exe () ;
  let code = FFI.from_string ~source ~filename in
  Py.finalize () ;
  (* Since Textual doesn't have a concept of toplevel code, we create a function for this code,
     with a non-denotable name, so we don't clash with existing python code *)
  let module_ = PyTrans.to_module ~sourcefile code in
  if typecheck then (
    let res = TextualTypeVerification.type_check module_ in
    match (res : TextualTypeVerification.type_check_result) with
    | Ok ->
        F.printf "%a" Textual.Module.pp module_
    | Type_errors errors ->
        let pp_error = TextualTypeVerification.pp_error sourcefile in
        F.printf "%a" Textual.Module.pp module_ ;
        F.printf "Errors while type checking the test:\n" ;
        List.iter errors ~f:(fun err -> F.printf "%a\n" pp_error err)
    | Decl_errors errors ->
        let pp_error = TextualDecls.pp_error sourcefile in
        F.printf "%a" Textual.Module.pp module_ ;
        F.printf "Errors while creating the decls:\n" ;
        List.iter errors ~f:(fun err -> F.printf "%a\n" pp_error err) )
  else F.printf "%a" Textual.Module.pp module_


let%test_module "basic_tests" =
  ( module struct
    let%expect_test _ =
      let source = "x = 42" in
      test source ;
      [%expect
        {|
        .source_language = "python"

        define dummy.$toplevel() : *PyObject {
          #b0:
              store &dummy::x <- $builtins.python_int(42):*PyInt
              ret null

        }

        global dummy::x: *PyObject

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


    let%expect_test _ =
      let source = {|
x = 42
print(x)
      |} in
      test source ;
      [%expect
        {|
        .source_language = "python"

        define dummy.$toplevel() : *PyObject {
          #b0:
              store &dummy::x <- $builtins.python_int(42):*PyInt
              n0:*PyInt = load &dummy::x
              n1 = $builtins.print(n0)
              ret null

        }

        global dummy::x: *PyObject

        declare $builtins.print(...) : *PyObject

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


    let%expect_test _ =
      let source = {|
x = 42
y = 10
print(x + y)
      |} in
      test source ;
      [%expect
        {|
        .source_language = "python"

        define dummy.$toplevel() : *PyObject {
          #b0:
              store &dummy::x <- $builtins.python_int(42):*PyInt
              store &dummy::y <- $builtins.python_int(10):*PyInt
              n0:*PyInt = load &dummy::x
              n1:*PyInt = load &dummy::y
              n2 = $builtins.binary_add(n0, n1)
              n3 = $builtins.print(n2)
              ret null

        }

        global dummy::y: *PyObject

        global dummy::x: *PyObject

        declare $builtins.print(...) : *PyObject

        declare $builtins.binary_add(*PyObject, *PyObject) : *PyObject

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


    let%expect_test _ =
      let source = {|
pi = 3.14
      |} in
      test source ;
      [%expect
        {|
        .source_language = "python"

        define dummy.$toplevel() : *PyObject {
          #b0:
              store &dummy::pi <- $builtins.python_float(3.14):*PyFloat
              ret null

        }

        global dummy::pi: *PyObject

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


    let%expect_test _ =
      let source = {|
byte_data = b'\x48\x65\x6C\x6C\x6F'  # Equivalent to b'Hello'
      |} in
      test source ;
      [%expect
        {|
        .source_language = "python"

        define dummy.$toplevel() : *PyObject {
          #b0:
              store &dummy::byte_data <- $builtins.python_bytes("Hello"):*PyBytes
              ret null

        }

        global dummy::byte_data: *PyObject

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]
  end )


let%test_module "top_level_calls" =
  ( module struct
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
        .source_language = "python"

        define dummy.$toplevel() : *PyObject {
          #b0:
              n0 = $builtins.python_code("dummy.my_fun")
              store &dummy::a <- $builtins.python_int(10):*PyInt
              n1:*PyInt = load &dummy::a
              n2 = dummy.my_fun($builtins.python_int(42), n1)
              store &dummy::z <- n2:*PyObject
              n3:*PyObject = load &dummy::z
              n4 = $builtins.print(n3)
              ret null

        }

        define dummy.my_fun(x: *PyObject, y: *PyObject) : *PyObject {
          local z: *PyObject
          #b0:
              n0:*PyObject = load &x
              n1 = $builtins.print(n0)
              n2:*PyObject = load &y
              n3 = $builtins.print(n2)
              n4:*PyObject = load &x
              n5:*PyObject = load &y
              n6 = $builtins.binary_add(n4, n5)
              store &z <- n6:*PyObject
              n7:*PyObject = load &z
              ret n7

        }

        global dummy::z: *PyObject

        global dummy::a: *PyObject

        declare $builtins.print(...) : *PyObject

        declare $builtins.python_code(*String) : *PyCode

        declare $builtins.binary_add(*PyObject, *PyObject) : *PyObject

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


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
        .source_language = "python"

        define dummy.$toplevel() : *PyObject {
          #b0:
              n0 = $builtins.python_code("dummy.update_global")
              store &dummy::z <- $builtins.python_int(0):*PyInt
              n1 = dummy.update_global()
              n2:*PyInt = load &dummy::z
              n3 = $builtins.print(n2)
              ret null

        }

        define dummy.update_global() : *PyObject {
          #b0:
              n0:*PyInt = load &dummy::z
              n1 = $builtins.binary_add(n0, $builtins.python_int(1))
              store &dummy::z <- n1:*PyObject
              ret null

        }

        global dummy::z: *PyObject

        declare $builtins.print(...) : *PyObject

        declare $builtins.python_code(*String) : *PyCode

        declare $builtins.binary_add(*PyObject, *PyObject) : *PyObject

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]
  end )


let%test_module "conditionals" =
  ( module struct
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
        .source_language = "python"

        define dummy.$toplevel() : *PyObject {
          #b0:
              n0 = $builtins.python_code("dummy.coin")
              n1 = $builtins.python_code("dummy.f")
              ret null

        }

        define dummy.coin() : *PyObject {
          #b0:
              ret $builtins.python_bool(0)

        }

        define dummy.f(x: *PyObject, y: *PyObject) : *PyObject {
          #b0:
              n0 = dummy.coin()
              n1 = $builtins.python_is_true(n0)
              jmp b1, b2

          #b1:
              prune n1
              n2:*PyObject = load &x
              ret n2

          #b2:
              prune __sil_lnot(n1)
              n3:*PyObject = load &y
              ret n3

          #b3:
              ret null

        }

        declare $builtins.python_code(*String) : *PyCode

        declare $builtins.python_is_true(*PyObject) : int

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


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
        .source_language = "python"

        define dummy.$toplevel() : *PyObject {
          #b0:
              n0 = $builtins.python_code("dummy.coin")
              n1 = $builtins.python_code("dummy.f")
              ret null

        }

        define dummy.coin() : *PyObject {
          #b0:
              ret $builtins.python_bool(0)

        }

        define dummy.f(x: *PyObject, y: *PyObject) : *PyObject {
          local z: *PyObject
          #b0:
              store &z <- $builtins.python_int(0):*PyInt
              n0 = dummy.coin()
              n1 = $builtins.python_is_true(n0)
              jmp b1, b2

          #b1:
              prune n1
              n2:*PyObject = load &x
              store &z <- n2:*PyObject
              jmp b3

          #b2:
              prune __sil_lnot(n1)
              n3:*PyObject = load &y
              store &z <- n3:*PyObject
              jmp b3

          #b3:
              n4:*PyObject = load &z
              ret n4

        }

        declare $builtins.python_code(*String) : *PyCode

        declare $builtins.python_is_true(*PyObject) : int

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


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
        .source_language = "python"

        define dummy.$toplevel() : *PyObject {
          #b0:
              n0 = $builtins.python_code("dummy.coin")
              n1 = $builtins.python_code("dummy.f")
              ret null

        }

        define dummy.coin() : *PyObject {
          #b0:
              ret $builtins.python_bool(0)

        }

        define dummy.f(x: *PyObject, y: *PyObject) : *PyObject {
          local z: *PyObject
          #b0:
              store &z <- $builtins.python_int(0):*PyInt
              n0 = dummy.coin()
              n1 = $builtins.python_is_true(n0)
              jmp b1, b2

          #b1:
              prune n1
              n2 = dummy.coin()
              n3 = $builtins.python_is_true(n2)
              jmp b3, b4

          #b3:
              prune n3
              n4:*PyObject = load &x
              store &z <- n4:*PyObject
              jmp b5

          #b4:
              prune __sil_lnot(n3)
              ret $builtins.python_int(1664)

          #b5:
              n5:*PyObject = load &z
              n6 = $builtins.binary_add(n5, $builtins.python_int(1))
              store &z <- n6:*PyObject
              jmp b6

          #b2:
              prune __sil_lnot(n1)
              n7:*PyObject = load &z
              n8 = $builtins.binary_add(n7, $builtins.python_int(1))
              store &z <- n8:*PyObject
              n9 = dummy.coin()
              n10 = $builtins.python_is_true(n9)
              jmp b7, b8

          #b7:
              prune n10
              ret $builtins.python_int(42)

          #b8:
              prune __sil_lnot(n10)
              n11:*PyObject = load &y
              store &z <- n11:*PyObject
              jmp b6

          #b6:
              n12:*PyObject = load &z
              ret n12

        }

        declare $builtins.python_code(*String) : *PyCode

        declare $builtins.binary_add(*PyObject, *PyObject) : *PyObject

        declare $builtins.python_is_true(*PyObject) : int

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


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
      .source_language = "python"

      define dummy.$toplevel() : *PyObject {
        #b0:
            n0 = $builtins.python_code("dummy.foo")
            n1 = $builtins.python_code("dummy.f")
            ret null

      }

      define dummy.foo(x: *PyObject) : *PyObject {
        #b0:
            ret null

      }

      define dummy.f(x: *PyObject) : *PyObject {
        #b0:
            n0:*PyObject = load &x
            n1 = $builtins.python_code("dummy.foo")
            n2 = $builtins.python_is_true(n0)
            jmp b1(n1), b2(n1)

        #b1(n3: *PyCode):
            prune n2
            jmp b3($builtins.python_int(1), n3)

        #b2(n4: *PyCode):
            prune __sil_lnot(n2)
            jmp b3($builtins.python_int(0), n4)

        #b3(n5: *PyInt, n6: *PyCode):
            n7 = $builtins.python_call(n6, n5)
            ret null

      }

      declare $builtins.python_code(*String) : *PyCode

      declare $builtins.python_call(...) : *PyObject

      declare $builtins.python_is_true(*PyObject) : int

      declare $builtins.python_tuple(...) : *PyObject

      declare $builtins.python_bytes(*Bytes) : *PyBytes

      declare $builtins.python_string(*String) : *PyString

      declare $builtins.python_bool(int) : *PyBool

      declare $builtins.python_float(float) : *PyFloat

      declare $builtins.python_int(int) : *PyInt |}]
  end )


let%test_module "iter" =
  ( module struct
    let%expect_test _ =
      let source = {|
for x in range(10):
    print(x)
      |} in
      test source ;
      [%expect
        {|
        .source_language = "python"

        define dummy.$toplevel() : *PyObject {
          #b0:
              n0 = $builtins.range($builtins.python_int(10))
              n1 = $builtins.python_iter(n0)
              jmp b1(n1)

          #b1(n2: *PyObject):
              n3 = $builtins.python_iter_next(n2)
              n4:int = load n3.PyIterItem.has_item
              jmp b2, b3

          #b2:
              prune n4
              n5:*PyObject = load n3.PyIterItem.next_item
              store &dummy::x <- n5:*PyObject
              n6:*PyObject = load &dummy::x
              n7 = $builtins.print(n6)
              jmp b1(n2)

          #b3:
              prune __sil_lnot(n4)
              ret null

        }

        global dummy::x: *PyObject

        declare $builtins.range(...) : *PyObject

        declare $builtins.print(...) : *PyObject

        declare $builtins.python_iter_next(*PyObject) : *PyIterItem

        type PyIterItem = {has_item: int; next_item: *PyObject}

        declare $builtins.python_iter(*PyObject) : *PyObject

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]
  end )


let%test_module "shadowing" =
  ( module struct
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
        .source_language = "python"

        define dummy.$toplevel() : *PyObject {
          #b0:
              n0 = $builtins.print($builtins.python_int(42))
              n1 = $builtins.python_code("dummy.print")
              n2 = dummy.print($builtins.python_int(42))
              n3 = $builtins.python_code("dummy.f")
              ret null

        }

        define dummy.print(x: *PyObject) : *PyObject {
          #b0:
              n0:*PyObject = load &x
              ret n0

        }

        define dummy.f(x: *PyObject) : *PyObject {
          #b0:
              n0:*PyObject = load &x
              n1 = dummy.print(n0)
              ret null

        }

        declare $builtins.print(...) : *PyObject

        declare $builtins.python_code(*String) : *PyCode

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]
  end )


let%test_module "toplevel typing" =
  ( module struct
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
        .source_language = "python"

        define dummy.$toplevel() : *PyObject {
          #b0:
              n0 = $builtins.python_code("dummy.f0")
              n1 = $builtins.python_code("dummy.f1")
              ret null

        }

        define dummy.f0(x: *PyInt, y: *PyObject, z: *PyFloat) : *PyObject {
          #b0:
              ret null

        }

        define dummy.f1(x: *PyObject, y: *PyString) : *PyBool {
          #b0:
              ret null

        }

        declare $builtins.python_code(*String) : *PyCode

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


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
        .source_language = "python"

        define dummy.$toplevel() : *PyObject {
          #b0:
              n0 = $builtins.python_code("dummy.expect_int")
              n1 = $builtins.python_code("dummy.get")
              n2 = dummy.get()
              n3 = dummy.expect_int(n2)
              ret null

        }

        define dummy.expect_int(x: *PyInt) : *PyObject {
          #b0:
              ret null

        }

        define dummy.get() : *PyInt {
          #b0:
              ret $builtins.python_int(42)

        }

        declare $builtins.python_code(*String) : *PyCode

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


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
        .source_language = "python"

        define dummy.$toplevel() : *PyObject {
          #b0:
              n0 = $builtins.python_code("dummy.expect")
              n1 = $builtins.python_code("dummy.get")
              n2 = dummy.get()
              n3 = dummy.expect(n2)
              ret null

        }

        define dummy.expect(x: *PyObject) : *PyNone {
          #b0:
              ret null

        }

        define dummy.get() : *PyInt {
          #b0:
              ret $builtins.python_int(42)

        }

        declare $builtins.python_code(*String) : *PyCode

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]
  end )


let%test_module "simple user classes" =
  ( module struct
    let%expect_test _ =
      let source =
        {|
class C:
        def __init__(self, x):
            self.x = x

        def get(self):
            return self.x

        def set(self, x):
            self.x = x

c = C()
c.x
c.get()
c.set(42)
        |}
      in
      test source ;
      [%expect
        {|
          .source_language = "python"

          define dummy.$toplevel() : *PyObject {
            #b0:
                n0 = $builtins.python_class("dummy::C")
                n1 = $builtins.python_class_constructor("dummy::C")
                store &dummy::c <- n1:*dummy::C
                n2:*dummy::C = load &dummy::c
                n3 = n2.?.x
                n4:*dummy::C = load &dummy::c
                n5 = $builtins.python_load_method(n4, "get")
                n6 = $builtins.python_call_method(n5)
                n7:*dummy::C = load &dummy::c
                n8 = $builtins.python_load_method(n7, "set")
                n9 = $builtins.python_call_method(n8, $builtins.python_int(42))
                ret null

          }

          define dummy::C.__init__(self: *PyObject, x: *PyObject) : *PyObject {
            #b0:
                n0:*PyObject = load &self
                n1:*PyObject = load &x
                store n0.?.x <- n1:*PyObject
                ret null

          }

          define dummy::C.get(self: *PyObject) : *PyObject {
            #b0:
                n0:*PyObject = load &self
                n1 = n0.?.x
                ret n1

          }

          define dummy::C.set(self: *PyObject, x: *PyObject) : *PyObject {
            #b0:
                n0:*PyObject = load &self
                n1:*PyObject = load &x
                store n0.?.x <- n1:*PyObject
                ret null

          }

          type dummy::C = {}

          global dummy::c: *PyObject

          declare $builtins.python_load_method(*PyObject, *String) : *PyMethod

          type PyMethod = {code: *PyCode; self: *PyObject}

          declare $builtins.python_class_constructor(...) : *PyObject

          declare $builtins.python_class(*String) : *PyClass

          declare $builtins.python_call_method(...) : *PyObject

          declare $builtins.python_tuple(...) : *PyObject

          declare $builtins.python_bytes(*Bytes) : *PyBytes

          declare $builtins.python_string(*String) : *PyString

          declare $builtins.python_bool(int) : *PyBool

          declare $builtins.python_float(float) : *PyFloat

          declare $builtins.python_int(int) : *PyInt |}]


    let%expect_test _ =
      let source =
        {|
class IntBox:
        x: int
        f: Callable[[int, bool, str], None]

        def __init__(self, x: int) -> None:
            self.x = x
            self.f = lambda i: lambda b: lambda s: print(42)

        def get(self) -> int:
            return self.x

        def set(self, x: int) -> None:
            self.x = x

        def run(self) -> None:
            self.f(3)(False)("yolo")

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
        .source_language = "python"

        define dummy.$toplevel() : *PyObject {
          #b0:
              n0 = $builtins.python_class("dummy::IntBox")
              n1 = $builtins.python_class_constructor("dummy::IntBox", $builtins.python_int(10))
              store &dummy::c <- n1:*dummy::IntBox
              n2:*dummy::IntBox = load &dummy::c
              n3 = n2.?.x
              n4:*dummy::IntBox = load &dummy::c
              store n4.?.z <- $builtins.python_int(10):*PyInt
              n5:*dummy::IntBox = load &dummy::c
              n6 = $builtins.python_load_method(n5, "get")
              n7 = $builtins.python_call_method(n6)
              n8:*dummy::IntBox = load &dummy::c
              n9 = $builtins.python_load_method(n8, "set")
              n10 = $builtins.python_call_method(n9, $builtins.python_int(42))
              n11:*dummy::IntBox = load &dummy::c
              n12 = $builtins.python_load_method(n11, "run")
              n13 = $builtins.python_call_method(n12)
              n14:*dummy::IntBox = load &dummy::c
              n15 = n14.?.z
              n16 = $builtins.print(n15)
              ret null

        }

        define dummy::IntBox.__init__(self: *PyObject, x: *PyInt) : *PyNone {
          #b0:
              n0:*PyObject = load &self
              n1:*PyObject = load &x
              store n0.?.x <- n1:*PyObject
              n2:*PyObject = load &self
              n3 = $builtins.python_code("<lambda>")
              store n2.?.f <- n3:*PyCode
              ret null

        }

        define dummy::IntBox.get(self: *PyObject) : *PyInt {
          #b0:
              n0:*PyObject = load &self
              n1 = n0.?.x
              ret n1

        }

        define dummy::IntBox.set(self: *PyObject, x: *PyInt) : *PyNone {
          #b0:
              n0:*PyObject = load &self
              n1:*PyObject = load &x
              store n0.?.x <- n1:*PyObject
              ret null

        }

        define dummy::IntBox.run(self: *PyObject) : *PyNone {
          #b0:
              n0:*PyObject = load &self
              n1 = $builtins.python_load_method(n0, "f")
              n2 = $builtins.python_call_method(n1, $builtins.python_int(3))
              n3 = $builtins.python_call(n2, $builtins.python_bool(0))
              n4 = $builtins.python_call(n3, $builtins.python_string("yolo"))
              ret null

        }

        type dummy::IntBox = {f: *PyObject; x: *PyInt}

        global dummy::c: *PyObject

        declare $builtins.print(...) : *PyObject

        declare $builtins.python_load_method(*PyObject, *String) : *PyMethod

        type PyMethod = {code: *PyCode; self: *PyObject}

        declare $builtins.python_code(*String) : *PyCode

        declare $builtins.python_class_constructor(...) : *PyObject

        declare $builtins.python_class(*String) : *PyClass

        declare $builtins.python_call_method(...) : *PyObject

        declare $builtins.python_call(...) : *PyObject

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]
  end )


let%test_module "simple import" =
  ( module struct
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
        .source_language = "python"

        define dummy.$toplevel() : *PyObject {
          #b0:
              n0 = base.$toplevel()
              n1 = base.f($builtins.python_int(0))
              ret null

        }

        declare base.f(...) : *PyObject

        declare base.$toplevel() : *PyObject

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


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
        .source_language = "python"

        define dummy.$toplevel() : *PyObject {
          #b0:
              n0 = $builtins.python_code("dummy.f")
              n1 = dummy.f()
              n2 = base.$toplevel()
              n3 = base.f()
              n4 = base.g()
              ret null

        }

        define dummy.f() : *PyObject {
          #b0:
              ret null

        }

        declare base.g(...) : *PyObject

        declare base.f(...) : *PyObject

        declare base.$toplevel() : *PyObject

        declare $builtins.python_code(*String) : *PyCode

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]
  end )


let%test_module "inheritance" =
  ( module struct
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
          .source_language = "python"

          define dummy.$toplevel() : *PyObject {
            #b0:
                n0 = $builtins.python_class("dummy::C")
                n1 = $builtins.python_class("dummy::D")
                ret null

          }

          type dummy::C = {}

          type dummy::D extends dummy::C = {}

          declare $builtins.python_class(*String) : *PyClass

          declare $builtins.python_tuple(...) : *PyObject

          declare $builtins.python_bytes(*Bytes) : *PyBytes

          declare $builtins.python_string(*String) : *PyString

          declare $builtins.python_bool(int) : *PyBool

          declare $builtins.python_float(float) : *PyFloat

          declare $builtins.python_int(int) : *PyInt |}]
  end )
