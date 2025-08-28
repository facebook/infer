(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2

let () = if not (Py.is_initialized ()) then Py.initialize ~interpreter:Version.python_exe ()

let assert_not expr = assert (Bool.equal expr false)

let test_basic_fun_good _ =
  let prog1 = "def f():\n  return 1" in
  let prog2 = "def f():\n  return 1" in
  assert (PythonCompareWithoutTypeAnnot.compare prog1 prog2)


let test_basic_fun_bad _ =
  let prog1 = "def f():\n  return 1" in
  let prog2 = "def f():\n  return 2" in
  assert_not (PythonCompareWithoutTypeAnnot.compare prog1 prog2)


let test_const_type_annot_good _ =
  let prog1 = {|
# pyre-unsafe

CATEGORIES_TO_REMOVE = {'a': 1, 'b': 2, 'c': 3}
|} in
  let prog2 =
    {|
# pyre-strict

CATEGORIES_TO_REMOVE: dict[str, int | None] = {'a': 1, 'b': 2, 'c': 3}
|}
  in
  assert (PythonCompareWithoutTypeAnnot.compare prog1 prog2)


let test_const_type_annot_bad _ =
  let prog1 = {|
# pyre-unsafe

CATEGORIES_TO_REMOVE = {'a': 1, 'b': 2, 'c': 3}
|} in
  let prog2 =
    {|
# pyre-strict

CATEGORIES_TO_REMOVE_RENAMED: dict[str, int | None] = {'a': 1, 'b': 2, 'c': 3}
|}
  in
  assert_not (PythonCompareWithoutTypeAnnot.compare prog1 prog2)


let test_fun_type_annot_good _ =
  let prog1 =
    {|
def foo():
    write_html(
        "file.json"
    )

def write_html(json_file_path): pass
|}
  in
  let prog2 =
    {|
# pyre-strict

def foo() -> None:
    write_html(
        "file.json"
    )

def write_html(json_file_path:str) -> None: pass
|}
  in
  assert (PythonCompareWithoutTypeAnnot.compare prog1 prog2)


let test_fun_type_annot_bad _ =
  let prog1 =
    {|
def foo():
    write_html(
        "file.json"
    )

def write_html(json_file_path): pass
|}
  in
  let prog2 =
    {|
# pyre-strict

def foo() -> None:
    write_html(
        "file_NEW.json"
    )

def write_html(json_file_path:str) -> None: pass
|}
  in
  assert_not (PythonCompareWithoutTypeAnnot.compare prog1 prog2)


(* We may want to silent reporting on case below with assertion added *)
let test_fun_with_assert_bad _ =
  let prog1 = {|
def foo(self) -> None:
    obj = self.obj
    x = obj.prop
|} in
  let prog2 =
    {|
def foo(self) -> None:
    obj = self.obj
    assert obj is not None
    x = obj.prop
|}
  in
  assert_not (PythonCompareWithoutTypeAnnot.compare prog1 prog2)


let test_with_import_good _ =
  let prog1 = {|
def greet(name):
    return f"Hello, {name}!"
|} in
  let prog2 =
    {|
# pyre-strict
from typing import Any

def greet(name: Any) -> str:
    return f"Hello, {name}!"
|}
  in
  assert (PythonCompareWithoutTypeAnnot.compare prog1 prog2)


let test_with_import_dir_good _ =
  let prog1 = {|
def main():
    print("Hello World!")
|} in
  let prog2 = {|
import urllib.parse
def main():
    print("Hello World!")
|} in
  assert (PythonCompareWithoutTypeAnnot.compare prog1 prog2)


let test_import_dir_alias_good _ =
  let prog1 = {|
def main():
    print("Hello World!")
|} in
  let prog2 = {|
import urllib.parse as parse
def main():
    print("Hello World!")
|} in
  assert (PythonCompareWithoutTypeAnnot.compare prog1 prog2)


let test_import_from_dir_good _ =
  let prog1 = {|
def main():
    print("Hello World!")
|} in
  let prog2 = {|
from urllib.parse import quote
def main():
    print("Hello World!")
|} in
  assert (PythonCompareWithoutTypeAnnot.compare prog1 prog2)


let test_import_from_dir_alias_good _ =
  let prog1 = {|
def main():
    print("Hello World!")
|} in
  let prog2 = {|
from urllib.parse import quote as q
def main():
    print("Hello World!")
|} in
  assert (PythonCompareWithoutTypeAnnot.compare prog1 prog2)


let test_import_from_dir_alias_bad _ =
  let prog1 = {|
def main():
    print("Hello World!")
|} in
  let prog2 =
    {|
from urllib.parse import quote as q
def main():
    print(1)
    print("Hello World!")
|}
  in
  assert_not (PythonCompareWithoutTypeAnnot.compare prog1 prog2)


let fn_test_with_import_bad _ =
  let prog1 =
    {|
def greet(name: str) -> str:
    return f"Hello, {name}!"
print(greet("Alice"))
print(greet.__annotations__)
|}
  in
  let prog2 =
    {|
from __future__ import annotations
def greet(name: str) -> str:
    return f"Hello, {name}!"
print(greet("Alice"))
print(greet.__annotations__)
|}
  in
  assert (PythonCompareWithoutTypeAnnot.compare prog1 prog2)


let test_change_async_fun_param_type_good _ =
  let prog1 = {|
# pyre-unsafe

async def foo(
  a: bool, b: bool, c
) -> bool:
  return True
|} in
  let prog2 =
    {|
# pyre-strict

async def foo(
  a: bool, b: bool, c: bool
) -> bool:
  return True
|}
  in
  assert (PythonCompareWithoutTypeAnnot.compare prog1 prog2)


let test_change_lambda_param_type_good _ =
  let prog1 = {|
square = lambda x: x * x
print(square(5))
|} in
  let prog2 =
    {|
from typing import Callable

square: Callable[[int], int] = lambda x: x * x
print(square(5))
|}
  in
  assert (PythonCompareWithoutTypeAnnot.compare prog1 prog2)


let suite =
  "PythonCompareWithoutTypeAnnotTest"
  >::: [ "test_basic_fun_good" >:: test_basic_fun_good
       ; "test_basic_fun_bad" >:: test_basic_fun_bad
       ; "test_const_type_annot_good" >:: test_const_type_annot_good
       ; "test_const_type_annot_bad" >:: test_const_type_annot_bad
       ; "test_fun_type_annot_good" >:: test_fun_type_annot_good
       ; "test_fun_type_annot_bad" >:: test_fun_type_annot_bad
       ; "test_fun_with_assert_bad" >:: test_fun_with_assert_bad
       ; "test_with_import_good" >:: test_with_import_good
       ; "test_with_import_dir_good" >:: test_with_import_dir_good
       ; "test_import_from_dir_good" >:: test_import_from_dir_good
       ; "test_import_dir_alias_good" >:: test_import_dir_alias_good
       ; "test_import_from_dir_alias_good" >:: test_import_from_dir_alias_good
       ; "fn_test_with_import_bad" >:: fn_test_with_import_bad
       ; "test_import_from_dir_alias_bad" >:: test_import_from_dir_alias_bad
       ; "test_change_async_fun_param_type_good" >:: test_change_async_fun_param_type_good
       ; "test_change_lambda_param_type_good" >:: test_change_lambda_param_type_good ]


let () = run_test_tt_main suite
