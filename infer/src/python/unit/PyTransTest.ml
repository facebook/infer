(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let sourcefile = Textual.SourceFile.create "dummy.py"

let test ?(typecheck = true) source =
  Py.initialize ~interpreter:Version.python_exe () ;
  let code = FFI.from_string ~source ~filename:"dummy" in
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

        define $module$::toplevel() : *PyObject {
          #b0:
              store &$module$::x <- $builtins.$python_int$(42):*PyInt
              ret null

        }

        global $module$::x: *PyObject

        declare $builtins.$python_tuple$(...) : *PyObject

        declare $builtins.$python_string$(*String) : *PyString

        declare $builtins.$python_bool$(int) : *PyBool

        declare $builtins.$python_int$(int) : *PyInt |}]


    let%expect_test _ =
      let source = {|
x = 42
print(x)
      |} in
      test source ;
      [%expect
        {|
        .source_language = "python"

        define $module$::toplevel() : *PyObject {
          #b0:
              store &$module$::x <- $builtins.$python_int$(42):*PyInt
              n0:*PyInt = load &$module$::x
              n1 = $builtins.print(n0)
              ret null

        }

        global $module$::x: *PyObject

        declare $builtins.print(...) : *PyObject

        declare $builtins.$python_tuple$(...) : *PyObject

        declare $builtins.$python_string$(*String) : *PyString

        declare $builtins.$python_bool$(int) : *PyBool

        declare $builtins.$python_int$(int) : *PyInt |}]


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

        define $module$::toplevel() : *PyObject {
          #b0:
              store &$module$::x <- $builtins.$python_int$(42):*PyInt
              store &$module$::y <- $builtins.$python_int$(10):*PyInt
              n0:*PyInt = load &$module$::x
              n1:*PyInt = load &$module$::y
              n2 = $builtins.$binary_add$(n0, n1)
              n3 = $builtins.print(n2)
              ret null

        }

        global $module$::y: *PyObject

        global $module$::x: *PyObject

        declare $builtins.print(...) : *PyObject

        declare $builtins.$binary_add$(*PyObject, *PyObject) : *PyObject

        declare $builtins.$python_tuple$(...) : *PyObject

        declare $builtins.$python_string$(*String) : *PyString

        declare $builtins.$python_bool$(int) : *PyBool

        declare $builtins.$python_int$(int) : *PyInt |}]
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

        define $module$::toplevel() : *PyObject {
          #b0:
              n0 = $builtins.$python_code$("my_fun")
              store &$module$::a <- $builtins.$python_int$(10):*PyInt
              n1:*PyInt = load &$module$::a
              n2 = $module$::my_fun($builtins.$python_int$(42), n1)
              store &$module$::z <- n2:*PyObject
              n3:*PyObject = load &$module$::z
              n4 = $builtins.print(n3)
              ret null

        }

        define $module$::my_fun(x: *PyObject, y: *PyObject) : *PyObject {
          local z: *PyObject
          #b0:
              n0:*PyObject = load &x
              n1 = $builtins.print(n0)
              n2:*PyObject = load &y
              n3 = $builtins.print(n2)
              n4:*PyObject = load &x
              n5:*PyObject = load &y
              n6 = $builtins.$binary_add$(n4, n5)
              store &z <- n6:*PyObject
              n7:*PyObject = load &z
              ret n7

        }

        global $module$::z: *PyObject

        global $module$::a: *PyObject

        declare $builtins.print(...) : *PyObject

        declare $builtins.$python_code$(*String) : *PyCode

        declare $builtins.$binary_add$(*PyObject, *PyObject) : *PyObject

        declare $builtins.$python_tuple$(...) : *PyObject

        declare $builtins.$python_string$(*String) : *PyString

        declare $builtins.$python_bool$(int) : *PyBool

        declare $builtins.$python_int$(int) : *PyInt |}]


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

        define $module$::toplevel() : *PyObject {
          #b0:
              n0 = $builtins.$python_code$("update_global")
              store &$module$::z <- $builtins.$python_int$(0):*PyInt
              n1 = $module$::update_global()
              n2:*PyInt = load &$module$::z
              n3 = $builtins.print(n2)
              ret null

        }

        define $module$::update_global() : *PyObject {
          #b0:
              n0:*PyInt = load &$module$::z
              n1 = $builtins.$binary_add$(n0, $builtins.$python_int$(1))
              store &$module$::z <- n1:*PyObject
              ret null

        }

        global $module$::z: *PyObject

        declare $builtins.print(...) : *PyObject

        declare $builtins.$python_code$(*String) : *PyCode

        declare $builtins.$binary_add$(*PyObject, *PyObject) : *PyObject

        declare $builtins.$python_tuple$(...) : *PyObject

        declare $builtins.$python_string$(*String) : *PyString

        declare $builtins.$python_bool$(int) : *PyBool

        declare $builtins.$python_int$(int) : *PyInt |}]
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

        define $module$::toplevel() : *PyObject {
          #b0:
              n0 = $builtins.$python_code$("coin")
              n1 = $builtins.$python_code$("f")
              ret null

        }

        define $module$::coin() : *PyObject {
          #b0:
              ret $builtins.$python_bool$(0)

        }

        define $module$::f(x: *PyObject, y: *PyObject) : *PyObject {
          #b0:
              n0 = $module$::coin()
              n1 = $builtins.$python_is_true$(n0)
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

        declare $builtins.$python_code$(*String) : *PyCode

        declare $builtins.$python_is_true$(*PyObject) : int

        declare $builtins.$python_tuple$(...) : *PyObject

        declare $builtins.$python_string$(*String) : *PyString

        declare $builtins.$python_bool$(int) : *PyBool

        declare $builtins.$python_int$(int) : *PyInt |}]


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

        define $module$::toplevel() : *PyObject {
          #b0:
              n0 = $builtins.$python_code$("coin")
              n1 = $builtins.$python_code$("f")
              ret null

        }

        define $module$::coin() : *PyObject {
          #b0:
              ret $builtins.$python_bool$(0)

        }

        define $module$::f(x: *PyObject, y: *PyObject) : *PyObject {
          local z: *PyObject
          #b0:
              store &z <- $builtins.$python_int$(0):*PyInt
              n0 = $module$::coin()
              n1 = $builtins.$python_is_true$(n0)
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

        declare $builtins.$python_code$(*String) : *PyCode

        declare $builtins.$python_is_true$(*PyObject) : int

        declare $builtins.$python_tuple$(...) : *PyObject

        declare $builtins.$python_string$(*String) : *PyString

        declare $builtins.$python_bool$(int) : *PyBool

        declare $builtins.$python_int$(int) : *PyInt |}]


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

        define $module$::toplevel() : *PyObject {
          #b0:
              n0 = $builtins.$python_code$("coin")
              n1 = $builtins.$python_code$("f")
              ret null

        }

        define $module$::coin() : *PyObject {
          #b0:
              ret $builtins.$python_bool$(0)

        }

        define $module$::f(x: *PyObject, y: *PyObject) : *PyObject {
          local z: *PyObject
          #b0:
              store &z <- $builtins.$python_int$(0):*PyInt
              n0 = $module$::coin()
              n1 = $builtins.$python_is_true$(n0)
              jmp b1, b2

          #b1:
              prune n1
              n2 = $module$::coin()
              n3 = $builtins.$python_is_true$(n2)
              jmp b3, b4

          #b3:
              prune n3
              n4:*PyObject = load &x
              store &z <- n4:*PyObject
              jmp b5

          #b4:
              prune __sil_lnot(n3)
              ret $builtins.$python_int$(1664)

          #b5:
              n5:*PyObject = load &z
              n6 = $builtins.$binary_add$(n5, $builtins.$python_int$(1))
              store &z <- n6:*PyObject
              jmp b6

          #b2:
              prune __sil_lnot(n1)
              n7:*PyObject = load &z
              n8 = $builtins.$binary_add$(n7, $builtins.$python_int$(1))
              store &z <- n8:*PyObject
              n9 = $module$::coin()
              n10 = $builtins.$python_is_true$(n9)
              jmp b7, b8

          #b7:
              prune n10
              ret $builtins.$python_int$(42)

          #b8:
              prune __sil_lnot(n10)
              n11:*PyObject = load &y
              store &z <- n11:*PyObject
              jmp b6

          #b6:
              n12:*PyObject = load &z
              ret n12

        }

        declare $builtins.$python_code$(*String) : *PyCode

        declare $builtins.$binary_add$(*PyObject, *PyObject) : *PyObject

        declare $builtins.$python_is_true$(*PyObject) : int

        declare $builtins.$python_tuple$(...) : *PyObject

        declare $builtins.$python_string$(*String) : *PyString

        declare $builtins.$python_bool$(int) : *PyBool

        declare $builtins.$python_int$(int) : *PyInt |}]


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

      define $module$::toplevel() : *PyObject {
        #b0:
            n0 = $builtins.$python_code$("foo")
            n1 = $builtins.$python_code$("f")
            ret null

      }

      define $module$::foo(x: *PyObject) : *PyObject {
        #b0:
            ret null

      }

      define $module$::f(x: *PyObject) : *PyObject {
        #b0:
            n0:*PyObject = load &x
            n1 = $builtins.$python_code$("$module$::foo")
            n2 = $builtins.$python_is_true$(n0)
            jmp b1(n1), b2(n1)

        #b1(n3: *PyCode):
            prune n2
            jmp b3($builtins.$python_int$(1), n3)

        #b2(n4: *PyCode):
            prune __sil_lnot(n2)
            jmp b3($builtins.$python_int$(0), n4)

        #b3(n5: *PyInt, n6: *PyCode):
            n7 = $builtins.$python_call$(n6, n5)
            ret null

      }

      declare $builtins.$python_code$(*String) : *PyCode

      declare $builtins.$python_call$(...) : *PyObject

      declare $builtins.$python_is_true$(*PyObject) : int

      declare $builtins.$python_tuple$(...) : *PyObject

      declare $builtins.$python_string$(*String) : *PyString

      declare $builtins.$python_bool$(int) : *PyBool

      declare $builtins.$python_int$(int) : *PyInt |}]
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

        define $module$::toplevel() : *PyObject {
          #b0:
              n0 = $builtins.range($builtins.$python_int$(10))
              n1 = $builtins.$python_iter$(n0)
              jmp b1(n1)

          #b1(n2: *PyObject):
              n3 = $builtins.$python_iter_next$(n2)
              jmp b2, b3

          #b2:
              prune n3
              n4 = $builtins.$python_iter_item$(n2)
              store &$module$::x <- n4:*PyObject
              n5:*PyObject = load &$module$::x
              n6 = $builtins.print(n5)
              jmp b1(n2)

          #b3:
              prune __sil_lnot(n3)
              ret null

        }

        global $module$::x: *PyObject

        declare $builtins.range(...) : *PyObject

        declare $builtins.print(...) : *PyObject

        declare $builtins.$python_iter_next$(*PyObject) : int

        declare $builtins.$python_iter_item$(*PyObject) : *PyObject

        declare $builtins.$python_iter$(*PyObject) : *PyObject

        declare $builtins.$python_tuple$(...) : *PyObject

        declare $builtins.$python_string$(*String) : *PyString

        declare $builtins.$python_bool$(int) : *PyBool

        declare $builtins.$python_int$(int) : *PyInt |}]
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

        define $module$::toplevel() : *PyObject {
          #b0:
              n0 = $builtins.print($builtins.$python_int$(42))
              n1 = $builtins.$python_code$("print")
              n2 = $module$::print($builtins.$python_int$(42))
              n3 = $builtins.$python_code$("f")
              ret null

        }

        define $module$::print(x: *PyObject) : *PyObject {
          #b0:
              n0:*PyObject = load &x
              ret n0

        }

        define $module$::f(x: *PyObject) : *PyObject {
          #b0:
              n0:*PyObject = load &x
              n1 = $module$::print(n0)
              ret null

        }

        declare $builtins.print(...) : *PyObject

        declare $builtins.$python_code$(*String) : *PyCode

        declare $builtins.$python_tuple$(...) : *PyObject

        declare $builtins.$python_string$(*String) : *PyString

        declare $builtins.$python_bool$(int) : *PyBool

        declare $builtins.$python_int$(int) : *PyInt |}]
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

        define $module$::toplevel() : *PyObject {
          #b0:
              n0 = $builtins.$python_code$("f0")
              n1 = $builtins.$python_code$("f1")
              ret null

        }

        define $module$::f0(x: *PyInt, y: *PyObject, z: *PyFloat) : *PyObject {
          #b0:
              ret null

        }

        define $module$::f1(x: *PyObject, y: *PyString) : *PyBool {
          #b0:
              ret null

        }

        declare $builtins.$python_code$(*String) : *PyCode

        declare $builtins.$python_tuple$(...) : *PyObject

        declare $builtins.$python_string$(*String) : *PyString

        declare $builtins.$python_bool$(int) : *PyBool

        declare $builtins.$python_int$(int) : *PyInt |}]


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

        define $module$::toplevel() : *PyObject {
          #b0:
              n0 = $builtins.$python_code$("expect_int")
              n1 = $builtins.$python_code$("get")
              n2 = $module$::get()
              n3 = $module$::expect_int(n2)
              ret null

        }

        define $module$::expect_int(x: *PyInt) : *PyObject {
          #b0:
              ret null

        }

        define $module$::get() : *PyInt {
          #b0:
              ret $builtins.$python_int$(42)

        }

        declare $builtins.$python_code$(*String) : *PyCode

        declare $builtins.$python_tuple$(...) : *PyObject

        declare $builtins.$python_string$(*String) : *PyString

        declare $builtins.$python_bool$(int) : *PyBool

        declare $builtins.$python_int$(int) : *PyInt |}]


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

        define $module$::toplevel() : *PyObject {
          #b0:
              n0 = $builtins.$python_code$("expect")
              n1 = $builtins.$python_code$("get")
              n2 = $module$::get()
              n3 = $module$::expect(n2)
              ret null

        }

        define $module$::expect(x: *PyObject) : *PyNone {
          #b0:
              ret null

        }

        define $module$::get() : *PyInt {
          #b0:
              ret $builtins.$python_int$(42)

        }

        declare $builtins.$python_code$(*String) : *PyCode

        declare $builtins.$python_tuple$(...) : *PyObject

        declare $builtins.$python_string$(*String) : *PyString

        declare $builtins.$python_bool$(int) : *PyBool

        declare $builtins.$python_int$(int) : *PyInt |}]
  end )
