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

let test ?(typecheck = true) ?(filename = dummy) source =
  let open IResult.Let_syntax in
  let sourcefile = Textual.SourceFile.create filename in
  if not (Py.is_initialized ()) then Py.initialize ~interpreter:Version.python_exe () ;
  let* code = Result.map_error ~f:PyTrans.Error.ffi @@ FFI.from_string ~source ~filename in
  Py.finalize () ;
  (* Since Textual doesn't have a concept of toplevel code, we create a function for this code,
     with a non-denotable name, so we don't clash with existing python code *)
  let module_ = PyTrans.to_module ~sourcefile code in
  match module_ with
  | Ok module_ ->
      if typecheck then (
        let res = TextualTypeVerification.type_check module_ in
        match (res : TextualTypeVerification.type_check_result) with
        | Ok module_ ->
            F.printf "%a" Textual.Module.pp module_ ;
            Ok ()
        | Type_errors errors ->
            let pp_error = TextualTypeVerification.pp_error sourcefile in
            F.printf "%a" Textual.Module.pp module_ ;
            F.printf "Errors while type checking the test:\n" ;
            List.iter errors ~f:(fun err -> F.printf "%a\n" pp_error err) ;
            Ok ()
        | Decl_errors errors ->
            let pp_error = TextualDecls.pp_error sourcefile in
            F.printf "%a" Textual.Module.pp module_ ;
            F.printf "Errors while creating the decls:\n" ;
            List.iter errors ~f:(fun err -> F.printf "%a\n" pp_error err) ;
            Ok () )
      else (
        F.printf "%a" Textual.Module.pp module_ ;
        Ok () )
  | Error (level, err) ->
      let log =
        match level with
        | InternalError ->
            L.internal_error
        | ExternalError ->
            L.external_error
        | UserError ->
            L.user_error
      in
      log "%a@\n" PyTrans.Error.pp_kind err ;
      Ok ()


let test ?(typecheck = true) ?(filename = dummy) source =
  try ignore (test ~typecheck ~filename source)
  with Py.E _ as e -> L.die ExternalError "Pyml exception: %s@\n" (Exn.to_string e)


let%test_module "basic_tests" =
  ( module struct
    let%expect_test _ =
      let source = "x = 42" in
      test source ;
      [%expect
        {|
        .source_language = "python"

        define dummy.$toplevel() : *PyNone {
          #b0:
              store &dummy::x <- $builtins.python_int(42):*PyInt
              ret null

        }

        global dummy::x: *PyObject

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

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

        define dummy.$toplevel() : *PyNone {
          #b0:
              store &dummy::x <- $builtins.python_int(42):*PyInt
              n0:*PyInt = load &dummy::x
              n1 = $builtins.print(n0)
              ret null

        }

        global dummy::x: *PyObject

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

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

        define dummy.$toplevel() : *PyNone {
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

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

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
x = 42
y = 10
print(x - y)
      |} in
      test source ;
      [%expect
        {|
          .source_language = "python"

          define dummy.$toplevel() : *PyNone {
            #b0:
                store &dummy::x <- $builtins.python_int(42):*PyInt
                store &dummy::y <- $builtins.python_int(10):*PyInt
                n0:*PyInt = load &dummy::x
                n1:*PyInt = load &dummy::y
                n2 = $builtins.binary_subtract(n0, n1)
                n3 = $builtins.print(n2)
                ret null

          }

          global dummy::y: *PyObject

          global dummy::x: *PyObject

          global $python_implicit_names::__name__: *PyString

          global $python_implicit_names::__file__: *PyString

          declare $builtins.print(...) : *PyObject

          declare $builtins.binary_subtract(*PyObject, *PyObject) : *PyObject

          declare $builtins.python_tuple(...) : *PyObject

          declare $builtins.python_bytes(*Bytes) : *PyBytes

          declare $builtins.python_string(*String) : *PyString

          declare $builtins.python_bool(int) : *PyBool

          declare $builtins.python_float(float) : *PyFloat

          declare $builtins.python_int(int) : *PyInt |}]


    let%expect_test _ =
      let source = {|
x = 42
x += 10
print(x)
      |} in
      test source ;
      [%expect
        {|
          .source_language = "python"

          define dummy.$toplevel() : *PyNone {
            #b0:
                store &dummy::x <- $builtins.python_int(42):*PyInt
                n0:*PyInt = load &dummy::x
                n1 = $builtins.inplace_add(n0, $builtins.python_int(10))
                store &dummy::x <- n1:*PyObject
                n2:*PyObject = load &dummy::x
                n3 = $builtins.print(n2)
                ret null

          }

          global dummy::x: *PyObject

          global $python_implicit_names::__name__: *PyString

          global $python_implicit_names::__file__: *PyString

          declare $builtins.print(...) : *PyObject

          declare $builtins.inplace_add(*PyObject, *PyObject) : *PyObject

          declare $builtins.python_tuple(...) : *PyObject

          declare $builtins.python_bytes(*Bytes) : *PyBytes

          declare $builtins.python_string(*String) : *PyString

          declare $builtins.python_bool(int) : *PyBool

          declare $builtins.python_float(float) : *PyFloat

          declare $builtins.python_int(int) : *PyInt |}]


    let%expect_test _ =
      let source = {|
x = 42
x -= 10
print(x)
      |} in
      test source ;
      [%expect
        {|
          .source_language = "python"

          define dummy.$toplevel() : *PyNone {
            #b0:
                store &dummy::x <- $builtins.python_int(42):*PyInt
                n0:*PyInt = load &dummy::x
                n1 = $builtins.inplace_subtract(n0, $builtins.python_int(10))
                store &dummy::x <- n1:*PyObject
                n2:*PyObject = load &dummy::x
                n3 = $builtins.print(n2)
                ret null

          }

          global dummy::x: *PyObject

          global $python_implicit_names::__name__: *PyString

          global $python_implicit_names::__file__: *PyString

          declare $builtins.print(...) : *PyObject

          declare $builtins.inplace_subtract(*PyObject, *PyObject) : *PyObject

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

        define dummy.$toplevel() : *PyNone {
          #b0:
              store &dummy::pi <- $builtins.python_float(3.14):*PyFloat
              ret null

        }

        global dummy::pi: *PyObject

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

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

        define dummy.$toplevel() : *PyNone {
          #b0:
              store &dummy::byte_data <- $builtins.python_bytes("Hello"):*PyBytes
              ret null

        }

        global dummy::byte_data: *PyObject

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

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

        define dummy.$toplevel() : *PyNone {
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

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

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

        define dummy.$toplevel() : *PyNone {
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

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

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

        define dummy.$toplevel() : *PyNone {
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
              if n1 then jmp b1 else jmp b2

          #b1:
              n2:*PyObject = load &x
              ret n2

          #b2:
              n3:*PyObject = load &y
              ret n3

        }

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

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

        define dummy.$toplevel() : *PyNone {
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
              if n1 then jmp b1 else jmp b2

          #b1:
              n2:*PyObject = load &x
              store &z <- n2:*PyObject
              jmp b3

          #b2:
              n3:*PyObject = load &y
              store &z <- n3:*PyObject
              jmp b3

          #b3:
              n4:*PyObject = load &z
              ret n4

        }

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

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

        define dummy.$toplevel() : *PyNone {
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
              if n1 then jmp b1 else jmp b2

          #b1:
              n2 = dummy.coin()
              n3 = $builtins.python_is_true(n2)
              if n3 then jmp b3 else jmp b4

          #b3:
              n4:*PyObject = load &x
              store &z <- n4:*PyObject
              jmp b5

          #b4:
              ret $builtins.python_int(1664)

          #b5:
              n5:*PyObject = load &z
              n6 = $builtins.binary_add(n5, $builtins.python_int(1))
              store &z <- n6:*PyObject
              jmp b6

          #b2:
              n7:*PyObject = load &z
              n8 = $builtins.binary_add(n7, $builtins.python_int(1))
              store &z <- n8:*PyObject
              n9 = dummy.coin()
              n10 = $builtins.python_is_true(n9)
              if n10 then jmp b7 else jmp b8

          #b7:
              ret $builtins.python_int(42)

          #b8:
              n11:*PyObject = load &y
              store &z <- n11:*PyObject
              jmp b6

          #b6:
              n12:*PyObject = load &z
              ret n12

        }

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

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

      define dummy.$toplevel() : *PyNone {
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
            if n2 then jmp b1(n1) else jmp b2(n1)

        #b1(n3: *PyCode):
            jmp b3($builtins.python_int(1), n3)

        #b2(n4: *PyCode):
            jmp b3($builtins.python_int(0), n4)

        #b3(n5: *PyInt, n6: *PyCode):
            n7 = $builtins.python_call(n6, n5)
            ret null

      }

      global $python_implicit_names::__name__: *PyString

      global $python_implicit_names::__file__: *PyString

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

        define dummy.$toplevel() : *PyNone {
          #b0:
              n0 = $builtins.range($builtins.python_int(10))
              n1 = $builtins.python_iter(n0)
              jmp b1(n1)

          #b1(n2: *PyObject):
              n3 = $builtins.python_iter_next(n2)
              n4:int = load n3.PyIterItem.has_item
              if n4 then jmp b2 else jmp b3

          #b2:
              n5:*PyObject = load n3.PyIterItem.next_item
              store &dummy::x <- n5:*PyObject
              n6:*PyObject = load &dummy::x
              n7 = $builtins.print(n6)
              jmp b1(n2)

          #b3:
              ret null

        }

        global dummy::x: *PyObject

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

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

        define dummy.$toplevel() : *PyNone {
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

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

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

        define dummy.$toplevel() : *PyNone {
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

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

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

        define dummy.$toplevel() : *PyNone {
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

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

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

        define dummy.$toplevel() : *PyNone {
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

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

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
        .source_language = "python"

        define dummy.$toplevel() : *PyNone {
          #b0:
              n0 = $builtins.python_class("dummy::C")
              n1 = dummy::C($builtins.python_int(0), $builtins.python_string("a"))
              store &dummy::c <- n1:*dummy::C
              n2:*dummy::C = load &dummy::c
              n3:*PyObject = load n2.?.x
              n4:*dummy::C = load &dummy::c
              n5 = n4.?.get()
              n6:*dummy::C = load &dummy::c
              n7 = n6.?.set($builtins.python_int(42))
              ret null

        }

        define dummy::C.__init__(self: *dummy::C, x: *PyObject, y: *PyObject) : *PyNone {
          #b0:
              n0:*dummy::C = load &self
              n1:*PyObject = load &x
              store n0.?.x <- n1:*PyObject
              n2:*dummy::C = load &self
              n3:*PyObject = load &y
              store n2.?.y <- n3:*PyObject
              ret null

        }

        define dummy::C.get(self: *dummy::C) : *PyObject {
          #b0:
              n0:*dummy::C = load &self
              n1:*PyObject = load n0.?.x
              ret n1

        }

        define dummy::C.set(self: *dummy::C, x: *PyObject) : *PyObject {
          #b0:
              n0:*dummy::C = load &self
              n1:*PyObject = load &x
              store n0.?.x <- n1:*PyObject
              ret null

        }

        define dummy::C(x: *PyObject, y: *PyObject) : *dummy::C {
          #entry:
              n0 = __sil_allocate(<dummy::C>)
              n1:*PyObject = load &x
              n2:*PyObject = load &y
              n3 = n0.dummy::C.__init__(n1, n2)
              ret n0

        }

        global dummy::C$static: *PyObject

        type .static dummy::C$static = {}

        type dummy::C = {}

        global dummy::c: *PyObject

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

        declare $builtins.python_class(*String) : *PyClass

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
        .source_language = "python"

        define dummy.$toplevel() : *PyNone {
          #b0:
              n0 = $builtins.python_class("dummy::IntBox")
              n1 = $builtins.python_code("dummy.getX")
              n2 = dummy::IntBox($builtins.python_int(10))
              store &dummy::c <- n2:*dummy::IntBox
              n3:*dummy::IntBox = load &dummy::c
              n4:*PyObject = load n3.?.x
              n5:*dummy::IntBox = load &dummy::c
              store n5.?.z <- $builtins.python_int(10):*PyInt
              n6:*dummy::IntBox = load &dummy::c
              n7 = n6.?.get()
              n8:*dummy::IntBox = load &dummy::c
              n9 = n8.?.set($builtins.python_int(42))
              n10:*dummy::IntBox = load &dummy::c
              n11 = n10.?.run()
              n12:*dummy::IntBox = load &dummy::c
              n13:*PyObject = load n12.?.z
              n14 = $builtins.print(n13)
              ret null

        }

        define dummy::IntBox$static.id(x: *PyInt) : *PyInt {
          #b0:
              n0:*PyInt = load &x
              ret n0

        }

        define dummy::IntBox.__init__(self: *dummy::IntBox, x: *PyInt) : *PyNone {
          #b0:
              n0:*dummy::IntBox = load &self
              n1:*PyInt = load &x
              store n0.?.x <- n1:*PyInt
              n2:*dummy::IntBox = load &self
              n3 = $builtins.python_code("$ambiguous.code")
              store n2.?.f <- n3:*PyCode
              ret null

        }

        define dummy::IntBox.get(self: *dummy::IntBox) : *PyInt {
          #b0:
              n0:*dummy::IntBox = load &self
              n1:*PyInt = load n0.?.x
              ret n1

        }

        define dummy::IntBox.set(self: *dummy::IntBox, x: *PyInt) : *PyNone {
          #b0:
              n0:*dummy::IntBox = load &self
              n1:*PyInt = load &x
              store n0.?.x <- n1:*PyInt
              ret null

        }

        define dummy::IntBox.run(self: *dummy::IntBox) : *PyNone {
          #b0:
              n0:*dummy::IntBox = load &self
              n1 = n0.?.f($builtins.python_int(3))
              n2 = $builtins.python_call(n1, $builtins.python_bool(0))
              n3 = $builtins.python_call(n2, $builtins.python_string("yolo"))
              ret null

        }

        define dummy::IntBox(x: *PyInt) : *dummy::IntBox {
          #entry:
              n0 = __sil_allocate(<dummy::IntBox>)
              n1:*PyInt = load &x
              n2 = n0.dummy::IntBox.__init__(n1)
              ret n0

        }

        global dummy::IntBox$static: *PyObject

        type .static dummy::IntBox$static = {}

        type dummy::IntBox = {f: *Callable[[int, bool, str], None]; x: *PyInt}

        define dummy.getX(box: *dummy::IntBox) : *PyInt {
          #b0:
              n0:*dummy::IntBox = load &box
              n1 = n0.?.get()
              ret n1

        }

        global dummy::c: *PyObject

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

        declare $builtins.print(...) : *PyObject

        declare $builtins.python_code(*String) : *PyCode

        declare $builtins.python_class(*String) : *PyClass

        declare $builtins.python_call(...) : *PyObject

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


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
        .source_language = "python"

        define dummy.$toplevel() : *PyNone {
          #b0:
              n0 = $builtins.python_class("dummy::C")
              n1 = $builtins.python_class("dummy::D")
              ret null

        }

        define dummy::C$static.f() : *PyObject {
          #b0:
              ret null

        }

        define dummy::C$static.typed_f(x: *PyInt) : *PyInt {
          #b0:
              n0:*PyInt = load &x
              ret n0

        }

        declare dummy::C(...) : *dummy::C

        declare dummy::C.__init__(...) : *PyNone

        global dummy::C$static: *PyObject

        type .static dummy::C$static = {}

        type dummy::C = {}

        declare dummy::D(...) : *dummy::D

        declare dummy::D.__init__(...) : *PyNone

        global dummy::D$static: *PyObject

        type .static dummy::D$static extends dummy::C$static = {}

        type dummy::D extends dummy::C = {}

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

        declare $builtins.python_class(*String) : *PyClass

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


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
        .source_language = "python"

        define dummy.$toplevel() : *PyNone {
          #b0:
              n0 = $builtins.python_class("dummy::C")
              n1 = dummy::C$static.f()
              ret null

        }

        define dummy::C$static.f() : *PyObject {
          #b0:
              ret null

        }

        declare dummy::C(...) : *dummy::C

        declare dummy::C.__init__(...) : *PyNone

        global dummy::C$static: *PyObject

        type .static dummy::C$static = {}

        type dummy::C = {}

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

        declare $builtins.python_class(*String) : *PyClass

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


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
    .source_language = "python"

    define dummy.$toplevel() : *PyNone {
      #b0:
          n0 = $builtins.python_class("dummy::A")
          n1 = $builtins.python_class("dummy::C")
          n2 = $builtins.python_code("dummy.g")
          ret null

    }

    define dummy::A.f(self: *dummy::A) : *PyObject {
      #b0:
          ret null

    }

    declare dummy::A(...) : *dummy::A

    declare dummy::A.__init__(...) : *PyNone

    global dummy::A$static: *PyObject

    type .static dummy::A$static = {}

    type dummy::A = {}

    declare dummy::C(...) : *dummy::C

    declare dummy::C.__init__(...) : *PyNone

    global dummy::C$static: *PyObject

    type .static dummy::C$static = {}

    type dummy::C = {a: *dummy::A}

    define dummy.g(c: *dummy::C) : *PyNone {
      #b0:
          n0:*dummy::C = load &c
          n1:*dummy::A = load n0.?.a
          n2 = n1.?.f()
          n3 = $builtins.print(n2)
          ret null

    }

    global $python_implicit_names::__name__: *PyString

    global $python_implicit_names::__file__: *PyString

    declare $builtins.print(...) : *PyObject

    declare $builtins.python_code(*String) : *PyCode

    declare $builtins.python_class(*String) : *PyClass

    declare $builtins.python_tuple(...) : *PyObject

    declare $builtins.python_bytes(*Bytes) : *PyBytes

    declare $builtins.python_string(*String) : *PyString

    declare $builtins.python_bool(int) : *PyBool

    declare $builtins.python_float(float) : *PyFloat

    declare $builtins.python_int(int) : *PyInt |}]
  end )


let%test_module "import" =
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

        define dummy.$toplevel() : *PyNone {
          #b0:
              n0 = base.$toplevel()
              n1 = base.f($builtins.python_int(0))
              ret null

        }

        declare base.f(...) : *PyObject

        declare base.$toplevel() : *PyObject

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

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

        define dummy.$toplevel() : *PyNone {
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

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

        declare $builtins.python_code(*String) : *PyCode

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


    let%expect_test _ =
      let source = {|
import unittest

class MyTest(unittest.TestCase):
        pass
        |} in
      test source ;
      [%expect
        {|
        .source_language = "python"

        define dummy.$toplevel() : *PyNone {
          #b0:
              n0 = unittest.$toplevel()
              n1 = $builtins.python_class("dummy::MyTest")
              ret null

        }

        declare dummy::MyTest(...) : *dummy::MyTest

        declare dummy::MyTest.__init__(...) : *PyNone

        global dummy::MyTest$static: *PyObject

        type .static dummy::MyTest$static extends unittest::TestCase$static = {
        }

        type dummy::MyTest extends unittest::TestCase = {}

        declare unittest.$toplevel() : *PyObject

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

        declare $builtins.python_class(*String) : *PyClass

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


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
      .source_language = "python"

      define dummy.$toplevel() : *PyNone {
        #b0:
            n0 = os.$toplevel()
            n1 = sys.$toplevel()
            n2 = test.libregrtest.$toplevel()
            n3:*PyObject = load &test.libregrtest::main
            store &dummy::main_in_temp_cwd <- n3:*PyObject
            n4 = $builtins.python_code("dummy._main")
            n5:*PyString = load &$python_implicit_names::__name__
            n6 = $builtins.python_eq(n5, $builtins.python_string("__main__"))
            n7 = $builtins.python_is_true(n6)
            if n7 then jmp b1 else jmp b2

        #b1:
            n8 = dummy._main()
            jmp b2

        #b2:
            ret null

      }

      define dummy._main() : *PyObject {
        local mydir: *PyObject, i: *PyObject
        #b0:
            n0:*PyObject = load &os::path
            n1:*PyObject = load &os::path
            n2:*PyObject = load &os::path
            n3:*PyObject = load &sys::argv
            n4 = $builtins.python_subscript_get(n3, $builtins.python_int(0))
            n5 = n2.?.dirname(n4)
            n6 = n1.?.normpath(n5)
            n7 = n0.?.abspath(n6)
            store &mydir <- n7:*PyObject
            n8:*PyObject = load &sys::path
            n9 = ?.len(n8)
            n10 = $builtins.binary_subtract(n9, $builtins.python_int(1))
            store &i <- n10:*PyObject
            jmp b1

        #b1:
            n11:*PyObject = load &i
            n12 = $builtins.python_ge(n11, $builtins.python_int(0))
            n13 = $builtins.python_is_true(n12)
            if n13 then jmp b2 else jmp b3

        #b2:
            n14:*PyObject = load &os::path
            n15:*PyObject = load &os::path
            n16:*PyObject = load &i
            n17:*PyObject = load &sys::path
            n18 = $builtins.python_subscript_get(n17, n16)
            n19 = n15.?.normpath(n18)
            n20 = n14.?.abspath(n19)
            n21:*PyObject = load &mydir
            n22 = $builtins.python_eq(n20, n21)
            n23 = $builtins.python_is_true(n22)
            if n23 then jmp b4 else jmp b5

        #b4:
            jmp b1

        #b5:
            n24:*PyObject = load &i
            n25 = $builtins.inplace_subtract(n24, $builtins.python_int(1))
            store &i <- n25:*PyObject
            jmp b1

        #b3:
            n26:*PyObject = load &os::path
            n27:*PyString = load &$python_implicit_names::__file__
            n28 = n26.?.abspath(n27)
            store &$python_implicit_names::__file__ <- n28:*PyObject
            n29 = test.libregrtest.main()
            ret null

      }

      global dummy::main_in_temp_cwd: *PyObject

      global sys::path: *PyObject

      global sys::argv: *PyObject

      global os::path: *PyObject

      declare test.libregrtest.main(...) : *PyObject

      declare test.libregrtest.$toplevel() : *PyObject

      declare sys.$toplevel() : *PyObject

      declare os.$toplevel() : *PyObject

      global $python_implicit_names::__name__: *PyString

      global $python_implicit_names::__file__: *PyString

      declare $builtins.python_ge(*PyObject, *PyObject) : *PyBool

      declare $builtins.python_eq(*PyObject, *PyObject) : *PyBool

      declare $builtins.python_subscript_get(*PyObject, *PyObject) : *PyObject

      declare $builtins.python_code(*String) : *PyCode

      declare $builtins.inplace_subtract(*PyObject, *PyObject) : *PyObject

      declare $builtins.binary_subtract(*PyObject, *PyObject) : *PyObject

      declare $builtins.python_is_true(*PyObject) : int

      declare $builtins.python_tuple(...) : *PyObject

      declare $builtins.python_bytes(*Bytes) : *PyBytes

      declare $builtins.python_string(*String) : *PyString

      declare $builtins.python_bool(int) : *PyBool

      declare $builtins.python_float(float) : *PyFloat

      declare $builtins.python_int(int) : *PyInt

      Errors while type checking the test:
      dummy.py, line 4, column 0: textual type error: variable test.libregrtest::main has not been declared
          |}]


    let%expect_test _ =
      let source = {|
from A import X
from .B import X
from ..C import X
      |} in
      test ~filename:"some/long/path/dummy.py" source ;
      [%expect
        {|
      .source_language = "python"

      define some::long::path::dummy.$toplevel() : *PyNone {
        #b0:
            n0 = A.$toplevel()
            n1 = some::long::path::B.$toplevel()
            n2 = some::long::C.$toplevel()
            ret null

      }

      declare some::long::path::B.$toplevel() : *PyObject

      declare some::long::C.$toplevel() : *PyObject

      declare A.$toplevel() : *PyObject

      global $python_implicit_names::__name__: *PyString

      global $python_implicit_names::__file__: *PyString

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

          define dummy.$toplevel() : *PyNone {
            #b0:
                n0 = $builtins.python_class("dummy::C")
                n1 = $builtins.python_class("dummy::D")
                ret null

          }

          declare dummy::C(...) : *dummy::C

          declare dummy::C.__init__(...) : *PyNone

          global dummy::C$static: *PyObject

          type .static dummy::C$static = {}

          type dummy::C = {}

          declare dummy::D(...) : *dummy::D

          declare dummy::D.__init__(...) : *PyNone

          global dummy::D$static: *PyObject

          type .static dummy::D$static extends dummy::C$static = {}

          type dummy::D extends dummy::C = {}

          global $python_implicit_names::__name__: *PyString

          global $python_implicit_names::__file__: *PyString

          declare $builtins.python_class(*String) : *PyClass

          declare $builtins.python_tuple(...) : *PyObject

          declare $builtins.python_bytes(*Bytes) : *PyBytes

          declare $builtins.python_string(*String) : *PyString

          declare $builtins.python_bool(int) : *PyBool

          declare $builtins.python_float(float) : *PyFloat

          declare $builtins.python_int(int) : *PyInt |}]


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
          .source_language = "python"

          define dummy.$toplevel() : *PyNone {
            #b0:
                n0 = $builtins.python_class("dummy::C")
                n1 = $builtins.python_class("dummy::D")
                n2 = $builtins.python_class("dummy::C0")
                n3 = $builtins.python_class("dummy::D0")
                ret null

          }

          declare dummy::C(...) : *dummy::C

          declare dummy::C.__init__(...) : *PyNone

          global dummy::C$static: *PyObject

          type .static dummy::C$static = {}

          type dummy::C = {}

          define dummy::D.__init__(self: *dummy::D) : *PyNone {
            #b0:
                n0:*dummy::D = load &self
                n1 = dummy::C.__init__(n0)
                ret null

          }

          define dummy::D() : *dummy::D {
            #entry:
                n0 = __sil_allocate(<dummy::D>)
                n1 = n0.dummy::D.__init__()
                ret n0

          }

          global dummy::D$static: *PyObject

          type .static dummy::D$static extends dummy::C$static = {}

          type dummy::D extends dummy::C = {}

          define dummy::C0.__init__(foo: *dummy::C0, x: *PyObject) : *PyNone {
            #b0:
                n0:*dummy::C0 = load &foo
                n1:*PyObject = load &x
                store n0.?.x <- n1:*PyObject
                ret null

          }

          define dummy::C0(x: *PyObject) : *dummy::C0 {
            #entry:
                n0 = __sil_allocate(<dummy::C0>)
                n1:*PyObject = load &x
                n2 = n0.dummy::C0.__init__(n1)
                ret n0

          }

          global dummy::C0$static: *PyObject

          type .static dummy::C0$static = {}

          type dummy::C0 = {}

          define dummy::D0.__init__(bar: *dummy::D0) : *PyNone {
            #b0:
                n0:*dummy::D0 = load &bar
                n1 = dummy::C0.__init__(n0, $builtins.python_int(42))
                ret null

          }

          define dummy::D0() : *dummy::D0 {
            #entry:
                n0 = __sil_allocate(<dummy::D0>)
                n1 = n0.dummy::D0.__init__()
                ret n0

          }

          global dummy::D0$static: *PyObject

          type .static dummy::D0$static extends dummy::C0$static = {}

          type dummy::D0 extends dummy::C0 = {}

          global $python_implicit_names::__name__: *PyString

          global $python_implicit_names::__file__: *PyString

          declare $builtins.python_class(*String) : *PyClass

          declare $builtins.python_tuple(...) : *PyObject

          declare $builtins.python_bytes(*Bytes) : *PyBytes

          declare $builtins.python_string(*String) : *PyString

          declare $builtins.python_bool(int) : *PyBool

          declare $builtins.python_float(float) : *PyFloat

          declare $builtins.python_int(int) : *PyInt

          MAKE_FUNCTION: support for closures is incomplete (D)
          MAKE_FUNCTION: support for closures is incomplete (D0) |}]
  end )


let%test_module "compare_op" =
  ( module struct
    let%expect_test _ =
      let source = {|
def f(x, y):
  return (x == y)
        |} in
      test source ;
      [%expect
        {|
        .source_language = "python"

        define dummy.$toplevel() : *PyNone {
          #b0:
              n0 = $builtins.python_code("dummy.f")
              ret null

        }

        define dummy.f(x: *PyObject, y: *PyObject) : *PyObject {
          #b0:
              n0:*PyObject = load &x
              n1:*PyObject = load &y
              n2 = $builtins.python_eq(n0, n1)
              ret n2

        }

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

        declare $builtins.python_eq(*PyObject, *PyObject) : *PyBool

        declare $builtins.python_code(*String) : *PyCode

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


    let%expect_test _ =
      let source = "True != False" in
      test source ;
      [%expect
        {|
        .source_language = "python"

        define dummy.$toplevel() : *PyNone {
          #b0:
              n0 = $builtins.python_neq($builtins.python_bool(1), $builtins.python_bool(0))
              ret null

        }

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

        declare $builtins.python_neq(*PyObject, *PyObject) : *PyBool

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


    let%expect_test _ =
      let source = {|
def f(x, y, z, t):
        return (x and y) or (z and t)
        |} in
      test source ;
      [%expect
        {|
          .source_language = "python"

          define dummy.$toplevel() : *PyNone {
            #b0:
                n0 = $builtins.python_code("dummy.f")
                ret null

          }

          define dummy.f(x: *PyObject, y: *PyObject, z: *PyObject, t: *PyObject) : *PyObject {
            #b0:
                n0:*PyObject = load &x
                n1 = $builtins.python_is_true(n0)
                if n1 then jmp b1 else jmp b2

            #b1:
                n2:*PyObject = load &y
                n3 = $builtins.python_is_true(n2)
                n4:*PyObject = load &y
                if !(n3) then jmp b2 else jmp b3(n4)

            #b2:
                n5:*PyObject = load &z
                n6 = $builtins.python_is_true(n5)
                n7:*PyObject = load &z
                if n6 then jmp b4 else jmp b3(n7)

            #b4:
                n8:*PyObject = load &t
                jmp b3(n8)

            #b3(n9: *PyObject):
                ret n9

          }

          global $python_implicit_names::__name__: *PyString

          global $python_implicit_names::__file__: *PyString

          declare $builtins.python_code(*String) : *PyCode

          declare $builtins.python_is_true(*PyObject) : int

          declare $builtins.python_tuple(...) : *PyObject

          declare $builtins.python_bytes(*Bytes) : *PyBytes

          declare $builtins.python_string(*String) : *PyString

          declare $builtins.python_bool(int) : *PyBool

          declare $builtins.python_float(float) : *PyFloat

          declare $builtins.python_int(int) : *PyInt |}]


    let%expect_test _ =
      let source = {|
def f(x, y):
  return (x > y)
        |} in
      test source ;
      [%expect
        {|
          .source_language = "python"

          define dummy.$toplevel() : *PyNone {
            #b0:
                n0 = $builtins.python_code("dummy.f")
                ret null

          }

          define dummy.f(x: *PyObject, y: *PyObject) : *PyObject {
            #b0:
                n0:*PyObject = load &x
                n1:*PyObject = load &y
                n2 = $builtins.python_gt(n0, n1)
                ret n2

          }

          global $python_implicit_names::__name__: *PyString

          global $python_implicit_names::__file__: *PyString

          declare $builtins.python_gt(*PyObject, *PyObject) : *PyBool

          declare $builtins.python_code(*String) : *PyCode

          declare $builtins.python_tuple(...) : *PyObject

          declare $builtins.python_bytes(*Bytes) : *PyBytes

          declare $builtins.python_string(*String) : *PyString

          declare $builtins.python_bool(int) : *PyBool

          declare $builtins.python_float(float) : *PyFloat

          declare $builtins.python_int(int) : *PyInt |}]


    let%expect_test _ =
      let source = {|
def f(x, y):
  return (x <= y)
        |} in
      test source ;
      [%expect
        {|
          .source_language = "python"

          define dummy.$toplevel() : *PyNone {
            #b0:
                n0 = $builtins.python_code("dummy.f")
                ret null

          }

          define dummy.f(x: *PyObject, y: *PyObject) : *PyObject {
            #b0:
                n0:*PyObject = load &x
                n1:*PyObject = load &y
                n2 = $builtins.python_le(n0, n1)
                ret n2

          }

          global $python_implicit_names::__name__: *PyString

          global $python_implicit_names::__file__: *PyString

          declare $builtins.python_le(*PyObject, *PyObject) : *PyBool

          declare $builtins.python_code(*String) : *PyCode

          declare $builtins.python_tuple(...) : *PyObject

          declare $builtins.python_bytes(*Bytes) : *PyBytes

          declare $builtins.python_string(*String) : *PyString

          declare $builtins.python_bool(int) : *PyBool

          declare $builtins.python_float(float) : *PyFloat

          declare $builtins.python_int(int) : *PyInt |}]
  end )


let%test_module "abc" =
  ( module struct
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
        .source_language = "python"

        define dummy.$toplevel() : *PyNone {
          #b0:
              n0 = abc.$toplevel()
              n1 = $builtins.python_class("dummy::C")
              ret null

        }

        declare dummy::C$static.get_static0() : *PyNone

        declare dummy::C$static.get_static1() : *PyNone

        declare dummy::C.get(*dummy::C) : *PyNone

        declare dummy::C(...) : *dummy::C

        declare dummy::C.__init__(...) : *PyNone

        global dummy::C$static: *PyObject

        type .static dummy::C$static = {}

        type dummy::C = {}

        declare abc.$toplevel() : *PyObject

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

        declare $builtins.python_class(*String) : *PyClass

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


    let%expect_test _ =
      let source = {|
l = [1, 2, 3]
print(l[0])
|} in
      test source ;
      [%expect
        {|
          .source_language = "python"

          define dummy.$toplevel() : *PyNone {
            #b0:
                n0 = $builtins.python_build_list($builtins.python_int(1), $builtins.python_int(2), $builtins.python_int(3))
                store &dummy::l <- n0:*PyList
                n1:*PyList = load &dummy::l
                n2 = $builtins.python_subscript_get(n1, $builtins.python_int(0))
                n3 = $builtins.print(n2)
                ret null

          }

          global dummy::l: *PyObject

          global $python_implicit_names::__name__: *PyString

          global $python_implicit_names::__file__: *PyString

          declare $builtins.print(...) : *PyObject

          declare $builtins.python_subscript_get(*PyObject, *PyObject) : *PyObject

          declare $builtins.python_build_list(...) : *PyList

          declare $builtins.python_tuple(...) : *PyObject

          declare $builtins.python_bytes(*Bytes) : *PyBytes

          declare $builtins.python_string(*String) : *PyString

          declare $builtins.python_bool(int) : *PyBool

          declare $builtins.python_float(float) : *PyFloat

          declare $builtins.python_int(int) : *PyInt |}]


    let%expect_test _ =
      let source = {|
l = [1, 2, 3]
x = 0
l[x] = 10
|} in
      test source ;
      [%expect
        {|
          .source_language = "python"

          define dummy.$toplevel() : *PyNone {
            #b0:
                n0 = $builtins.python_build_list($builtins.python_int(1), $builtins.python_int(2), $builtins.python_int(3))
                store &dummy::l <- n0:*PyList
                store &dummy::x <- $builtins.python_int(0):*PyInt
                n1:*PyInt = load &dummy::x
                n2:*PyList = load &dummy::l
                n3 = $builtins.python_subscript_set(n2, n1, $builtins.python_int(10))
                ret null

          }

          global dummy::x: *PyObject

          global dummy::l: *PyObject

          global $python_implicit_names::__name__: *PyString

          global $python_implicit_names::__file__: *PyString

          declare $builtins.python_subscript_set(*PyObject, *PyObject, *PyObject) : *PyNone

          declare $builtins.python_build_list(...) : *PyList

          declare $builtins.python_tuple(...) : *PyObject

          declare $builtins.python_bytes(*Bytes) : *PyBytes

          declare $builtins.python_string(*String) : *PyString

          declare $builtins.python_bool(int) : *PyBool

          declare $builtins.python_float(float) : *PyFloat

          declare $builtins.python_int(int) : *PyInt |}]


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
        .source_language = "python"

        define dummy.$toplevel() : *PyNone {
          #b0:
              store &dummy::t <- $builtins.python_tuple($builtins.python_int(1), $builtins.python_int(2), $builtins.python_int(3)):*PyTuple
              n0 = $builtins.python_code("dummy.f")
              ret null

        }

        define dummy.f(x: *PyObject, y: *PyObject, z: *PyObject) : *PyObject {
          #b0:
              n0:*PyObject = load &x
              n1:*PyObject = load &y
              n2:*PyObject = load &z
              n3 = $builtins.python_build_tuple(n0, n1, n2)
              ret n3

        }

        global dummy::t: *PyObject

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

        declare $builtins.python_build_tuple(...) : *PyTuple

        declare $builtins.python_code(*String) : *PyCode

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


    let%expect_test _ =
      let source = {|
s = {1, 2, 3}
|} in
      test source ;
      [%expect
        {|
          .source_language = "python"

          define dummy.$toplevel() : *PyNone {
            #b0:
                n0 = $builtins.python_build_set($builtins.python_int(1), $builtins.python_int(2), $builtins.python_int(3))
                store &dummy::s <- n0:*PySet
                ret null

          }

          global dummy::s: *PyObject

          global $python_implicit_names::__name__: *PyString

          global $python_implicit_names::__file__: *PyString

          declare $builtins.python_build_set(...) : *PySet

          declare $builtins.python_tuple(...) : *PyObject

          declare $builtins.python_bytes(*Bytes) : *PyBytes

          declare $builtins.python_string(*String) : *PyString

          declare $builtins.python_bool(int) : *PyBool

          declare $builtins.python_float(float) : *PyFloat

          declare $builtins.python_int(int) : *PyInt |}]
  end )


let%test_module "collections" =
  ( module struct
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
        .source_language = "python"

        define dummy.$toplevel() : *PyNone {
          #b0:
              n0 = $builtins.python_build_list($builtins.python_int(1), $builtins.python_int(2), $builtins.python_int(3))
              store &dummy::l <- n0:*PyList
              n1:*PyList = load &dummy::l
              n2 = $builtins.print(n1)
              n3 = $builtins.python_code("dummy.build_list")
              ret null

        }

        define dummy.build_list() : *PyObject {
          #b0:
              n0 = $builtins.python_build_list($builtins.python_int(1), $builtins.python_int(2), $builtins.python_int(3))
              ret n0

        }

        global dummy::l: *PyObject

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

        declare $builtins.print(...) : *PyObject

        declare $builtins.python_build_list(...) : *PyList

        declare $builtins.python_code(*String) : *PyCode

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]
  end )


let%test_module "ad-hoc support" =
  ( module struct
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
  .source_language = "python"

  define dummy.$toplevel() : *PyNone {
    #b0:
        n0 = unittest.$toplevel()
        n1 = signal.$toplevel()
        n2 = 0
        n3 = unittest.skipUnless(n2, $builtins.python_string("requires setitimer()"))
        n4 = $builtins.python_class("dummy::Test")
        n5 = $builtins.python_call(n3, n4)
        store &dummy::Test <- n5:*PyObject
        ret null

  }

  declare dummy::Test(...) : *dummy::Test

  declare dummy::Test.__init__(...) : *PyNone

  global dummy::Test$static: *PyObject

  type .static dummy::Test$static extends unittest::TestCase$static = {
  }

  type dummy::Test extends unittest::TestCase = {}

  global dummy::Test: *PyObject

  declare unittest.skipUnless(...) : *PyObject

  declare unittest.$toplevel() : *PyObject

  declare signal.$toplevel() : *PyObject

  global $python_implicit_names::__name__: *PyString

  global $python_implicit_names::__file__: *PyString

  declare $builtins.python_class(*String) : *PyClass

  declare $builtins.python_call(...) : *PyObject

  declare $builtins.python_tuple(...) : *PyObject

  declare $builtins.python_bytes(*Bytes) : *PyBytes

  declare $builtins.python_string(*String) : *PyString

  declare $builtins.python_bool(int) : *PyBool

  declare $builtins.python_float(float) : *PyFloat

  declare $builtins.python_int(int) : *PyInt

  no support for `hasattr` at the moment.  Skipping... |}]
  end )


let%test_module "with/finally" =
  ( module struct
    let%expect_test _ =
      (* No with for this one it's a baseline to support [open] *)
      let source = {|
fp = open("foo.txt", "wt")
fp.write("yolo")
          |} in
      test source ;
      [%expect
        {|
        .source_language = "python"

        define dummy.$toplevel() : *PyNone {
          #b0:
              n0 = $builtins.open($builtins.python_string("foo.txt"), $builtins.python_string("wt"))
              store &dummy::fp <- n0:*PyObject
              n1:*PyObject = load &dummy::fp
              n2 = n1.?.write($builtins.python_string("yolo"))
              ret null

        }

        global dummy::fp: *PyObject

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

        declare $builtins.open(...) : *PyObject

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


    let%expect_test _ =
      let source = {|
with open("foo.txt", "wt") as fp:
    fp.write("yolo")
          |} in
      test source ;
      [%expect
        {|
        .source_language = "python"

        define dummy.$toplevel() : *PyNone {
          #b0:
              n0 = $builtins.open($builtins.python_string("foo.txt"), $builtins.python_string("wt"))
              n1 = n0.?.__enter__()
              store &dummy::fp <- n1:*PyObject
              n2:*PyObject = load &dummy::fp
              n3 = n2.?.write($builtins.python_string("yolo"))
              n4 = n0.?.__exit__(null, null, null)
              ret null

        }

        global dummy::fp: *PyObject

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

        declare $builtins.open(...) : *PyObject

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


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
      .source_language = "python"

      define dummy.$toplevel() : *PyNone {
        #b0:
            n0 = $builtins.print($builtins.python_string("TRY BLOCK"))
            n1 = $builtins.print($builtins.python_string("FINALLY BLOCK"))
            ret null

      }

      global $python_implicit_names::__name__: *PyString

      global $python_implicit_names::__file__: *PyString

      declare $builtins.print(...) : *PyObject

      declare $builtins.python_tuple(...) : *PyObject

      declare $builtins.python_bytes(*Bytes) : *PyBytes

      declare $builtins.python_string(*String) : *PyString

      declare $builtins.python_bool(int) : *PyBool

      declare $builtins.python_float(float) : *PyFloat

      declare $builtins.python_int(int) : *PyInt |}]
  end )


let%test_module "kwargs" =
  ( module struct
    let%expect_test _ =
      let source = {|
(a, b) = f()
|} in
      test source ;
      [%expect
        {|
        .source_language = "python"

        define dummy.$toplevel() : *PyNone {
          #b0:
              n0 = ?.f()
              n1 = $builtins.python_index(n0, 1)
              n2 = $builtins.python_index(n0, 0)
              store &dummy::a <- n2:*PyObject
              store &dummy::b <- n1:*PyObject
              ret null

        }

        global dummy::b: *PyObject

        global dummy::a: *PyObject

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

        declare $builtins.python_index(*PyObject, int) : *PyObject

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]


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
        .source_language = "python"

        define dummy.$toplevel() : *PyNone {
          #b0:
              n0 = $builtins.python_code("dummy.f")
              ret null

        }

        define dummy.f() : *PyObject {
          local kwargs: *PyObject, k: *PyObject, v: *PyObject
          #b0:
              n0:*PyObject = load &kwargs
              n1 = n0.?.items()
              n2 = $builtins.python_iter(n1)
              jmp b1(n2)

          #b1(n3: *PyObject):
              n4 = $builtins.python_iter_next(n3)
              n5:int = load n4.PyIterItem.has_item
              if n5 then jmp b2 else jmp b3

          #b2:
              n6:*PyObject = load n4.PyIterItem.next_item
              n7 = $builtins.python_index(n6, 1)
              n8 = $builtins.python_index(n6, 0)
              store &k <- n8:*PyObject
              store &v <- n7:*PyObject
              n9:*PyObject = load &k
              n10:*PyObject = load &v
              n11 = $builtins.print(n9, n10)
              jmp b1(n3)

          #b3:
              ret null

        }

        global $python_implicit_names::__name__: *PyString

        global $python_implicit_names::__file__: *PyString

        declare $builtins.print(...) : *PyObject

        declare $builtins.python_index(*PyObject, int) : *PyObject

        declare $builtins.python_iter_next(*PyObject) : *PyIterItem

        type PyIterItem = {has_item: int; next_item: *PyObject}

        declare $builtins.python_iter(*PyObject) : *PyObject

        declare $builtins.python_code(*String) : *PyCode

        declare $builtins.python_tuple(...) : *PyObject

        declare $builtins.python_bytes(*Bytes) : *PyBytes

        declare $builtins.python_string(*String) : *PyString

        declare $builtins.python_bool(int) : *PyBool

        declare $builtins.python_float(float) : *PyFloat

        declare $builtins.python_int(int) : *PyInt |}]
  end )
