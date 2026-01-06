(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let () = if not (Py.is_initialized ()) then Py.initialize ~interpreter:Version.python_exe ()

let ast_diff_equal ?(require_type_comparisons = false) prog1 prog2 =
  let test_eqsat = not require_type_comparisons in
  let diffs = PythonCompareWithoutTypeAnnot.test_ast_diff ~debug:false ~test_eqsat prog1 prog2 in
  List.iter diffs ~f:(F.printf "%a\n" Diff.pp_explicit)


let%expect_test "test_basic_fun_good" =
  let prog1 = "def f():\n  return 1" in
  let prog2 = "def f():\n  return 1" in
  ast_diff_equal prog1 prog2 ;
  [%expect {||}]


let%expect_test "test_basic_fun_bad" =
  let prog1 = "def f():\n  return 1" in
  let prog2 = "def f():\n  return 2" in
  ast_diff_equal prog1 prog2 ;
  [%expect {| (Line 2) -   return 1, +   return 2 |}]


let%expect_test "test_const_type_annot_good" =
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
  ast_diff_equal prog1 prog2 ;
  [%expect {||}]


let%expect_test "test_const_type_annot_bad" =
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
  ast_diff_equal prog1 prog2 ;
  [%expect
    {| (Line 4) - CATEGORIES_TO_REMOVE = {'a': 1, 'b': 2, 'c': 3}, + CATEGORIES_TO_REMOVE_RENAMED: dict[str, int | None] = {'a': 1, 'b': 2, 'c': 3} |}]


let%expect_test "test_fun_type_annot_good" =
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
  ast_diff_equal prog1 prog2 ;
  [%expect {||}]


let%expect_test "test_fun_type_annot_bad" =
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
  ast_diff_equal prog1 prog2 ;
  [%expect {|
    (Line 4) -         "file.json"
    (Line 6) +         "file_NEW.json"
    |}]


(* We may want to silent reporting on case below with assertion added *)
let%expect_test "test_fun_with_assert_bad" =
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
  ast_diff_equal prog1 prog2 ;
  [%expect {| (Line 4) +     assert obj is not None |}]


let%expect_test "test_with_import_good" =
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
  ast_diff_equal prog1 prog2 ;
  [%expect {||}]


let%expect_test "test_with_import_dir_good" =
  let prog1 = {|
def main():
    print("Hello World!")
|} in
  let prog2 = {|
import urllib.parse
def main():
    print("Hello World!")
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {||}]


let%expect_test "test_import_dir_alias_good" =
  let prog1 = {|
def main():
    print("Hello World!")
|} in
  let prog2 = {|
import urllib.parse as parse
def main():
    print("Hello World!")
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {||}]


let%expect_test "test_import_from_dir_good" =
  let prog1 = {|
def main():
    print("Hello World!")
|} in
  let prog2 = {|
from urllib.parse import quote
def main():
    print("Hello World!")
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {||}]


let%expect_test "test_import_from_dir_alias_good" =
  let prog1 = {|
def main():
    print("Hello World!")
|} in
  let prog2 = {|
from urllib.parse import quote as q
def main():
    print("Hello World!")
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {||}]


let%expect_test "test_import_from_dir_alias_bad" =
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
  ast_diff_equal prog1 prog2 ;
  [%expect {| (Line 4) +     print(1) |}]


let%expect_test "fn_test_with_import_bad" =
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
  ast_diff_equal prog1 prog2 ;
  [%expect {||}]


let%expect_test "test_change_async_fun_param_type_good" =
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
  ast_diff_equal prog1 prog2 ;
  [%expect {||}]


let%expect_test "test_change_lambda_param_type_good" =
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
  ast_diff_equal prog1 prog2 ;
  [%expect {||}]


let%expect_test "test_change_async_fun_body_bad" =
  let prog1 = {|
async def authenticate(self, token: str, tag: str) -> None:
        print(1)
|} in
  let prog2 = {|
async def authenticate(self, token: str, tag: str) -> None:
        print(2)
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {| (Line 3) -         print(1), +         print(2) |}]


let%expect_test "test_change_class_to_is_instance_good" =
  let prog1 = {|
def foo(self, x):
        if x.__class__ == str:
            print(1)
|} in
  let prog2 = {|
def foo(self, x) -> None:
        if isinstance(x, str):
            print(1)
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {||}]


let%expect_test "test_change_class_to_is_instance_bad" =
  let prog1 = {|
def foo(self, x):
        if x.__class__ == str:
            print(1)
|} in
  let prog2 =
    {|
def foo(self, x) -> None:
        if isinstance(x, None):
            print(1)
|}
  in
  ast_diff_equal prog1 prog2 ;
  [%expect {| (Line 3) -         if x.__class__ == str:, +         if isinstance(x, None): |}]


let%expect_test "test_change_async_body_indentation_bad" =
  let prog1 = {|
async def foo(self, x):
    if x == 1:
        print(1)
        print(2)
|} in
  let prog2 = {|
async def foo(self, x):
    if x == 1:
        print(1)
    print(2)
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {| (Line 5) -         print(2), +     print(2) |}]


let%expect_test "test_change_async_def_kwargs_good" =
  let prog1 = {|
async def foo(self, **kwargs): pass
|} in
  let prog2 = {|
async def foo(self, **kwargs: int): pass
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {||}]


let%expect_test "test_change_fun_type_bad" =
  let prog1 = {|
def foo(self, x: int) -> None: pass
|} in
  let prog2 = {|
def foo(self, x: int | None) -> None: pass
|} in
  ast_diff_equal ~require_type_comparisons:true prog1 prog2 ;
  [%expect
    {| (Line 2) - def foo(self, x: int) -> None: pass, + def foo(self, x: int | None) -> None: pass |}]


let%expect_test "test_change_async_fun_type_bad" =
  let prog1 = {|
async def foo(self, x: int) -> None: pass
|} in
  let prog2 = {|
async def foo(self, x: str) -> None: pass
|} in
  ast_diff_equal ~require_type_comparisons:true prog1 prog2 ;
  [%expect
    {| (Line 2) - async def foo(self, x: int) -> None: pass, + async def foo(self, x: str) -> None: pass |}]


let%expect_test "test_change_assign_type_bad" =
  let prog1 =
    {|
# pyre-unsafe

CATEGORIES_TO_REMOVE: dict[str, int] = {'a': 1, 'b': 2, 'c': 3}
|}
  in
  let prog2 =
    {|
# pyre-strict

CATEGORIES_TO_REMOVE: dict[str, int | None] = {'a': 1, 'b': 2, 'c': 3}
|}
  in
  ast_diff_equal ~require_type_comparisons:true prog1 prog2 ;
  [%expect
    {| (Line 4) - CATEGORIES_TO_REMOVE: dict[str, int] = {'a': 1, 'b': 2, 'c': 3}, + CATEGORIES_TO_REMOVE: dict[str, int | None] = {'a': 1, 'b': 2, 'c': 3} |}]


let%expect_test "test_change_fun_type_float_good" =
  let prog1 = {|
def foo(x) -> None: pass
|} in
  let prog2 = {|
def foo(x:float) -> None: pass
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {||}]


let%expect_test "test_change_fun_type_ellipsis_good" =
  let prog1 = {|
def foo(f) -> None: pass
|} in
  let prog2 = {|
from typing import Callable

def foo(f: Callable[..., int]) -> None: pass
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {||}]


let%expect_test "test_change_type_case_sensitive_ret_good" =
  let prog1 = {|
from typing import Dict
def foo() -> Dict[str, str]: pass
|} in
  let prog2 = {|
def foo() -> dict[str, str]: pass
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {||}]


let%expect_test "test_change_type_case_sensitive_param_good" =
  let prog1 = {|
from typing import Dict
def foo(x: Dict[str, str]) -> None: pass
|} in
  let prog2 = {|
def foo(x: dict[str, str]) -> None: pass
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {||}]


let%expect_test "test_change_type_case_sensitive_bad" =
  let prog1 = {|
from mylib import SomeUserDefinedType
def foo() -> SomeUserDefinedType: pass
|} in
  let prog2 = {|
from mylib import someUserDefinedType
def foo() -> someUserDefinedType: pass
|} in
  ast_diff_equal ~require_type_comparisons:true prog1 prog2 ;
  [%expect
    {| (Line 3) - def foo() -> SomeUserDefinedType: pass, + def foo() -> someUserDefinedType: pass |}]


let%expect_test "fp_test_initialisation_set_good" =
  let prog1 = {|
self.errors = {}
  |} in
  let prog2 = {|
self.errors: set[str] = set()
  |} in
  ast_diff_equal prog1 prog2 ;
  [%expect {| (Line 2) - self.errors = {}, + self.errors: set[str] = set() |}]


let%expect_test "fp_equivalent_logic_good" =
  let prog1 =
    {|
def foo():
  if self.field and self.field.label != TOP:
    return None
  return 0
  |}
  in
  let prog2 =
    {|
def foo():
  field = self.field
  if field and field.label != TOP:
    return None
  return 0
  |}
  in
  ast_diff_equal prog1 prog2 ;
  [%expect
    {|
    (Line 3) -   if self.field and self.field.label != TOP:, +   field = self.field
    (Line 4) +   if field and field.label != TOP:
    |}]


let%expect_test "fp_type_annotation_with_quotes_good" =
  let prog1 = {|
def foo(params) -> "Tree": pass
|} in
  let prog2 = {|
def foo(params: list[str]) -> Tree: pass
|} in
  ast_diff_equal ~require_type_comparisons:true prog1 prog2 ;
  [%expect
    {| (Line 2) - def foo(params) -> "Tree": pass, + def foo(params: list[str]) -> Tree: pass |}]


let%expect_test "noise_in_diff_bad" =
  let prog1 = {|
def foo() -> None:
  x = 1
  y = 2
  z = 3
  d = 4
  e = 5
|} in
  let prog2 = {|
def foo() -> None:
  r = 2
  x = 1
  y = 2
  z = 3
  d = 4
  e = 5
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {| (Line 3) +   r = 2 |}]


let%expect_test "fp_test_change_optional_type_good" =
  let prog1 = {|
def foo(x: Optional[int]) -> None: pass
|} in
  let prog2 = {|
def foo(x: int | None) -> None: pass
|} in
  ast_diff_equal ~require_type_comparisons:true prog1 prog2 ;
  [%expect
    {| (Line 2) - def foo(x: Optional[int]) -> None: pass, + def foo(x: int | None) -> None: pass |}]


let%expect_test "test_change_optional_type_bad" =
  let prog1 = {|
def foo(x: Optional[int]) -> None: pass
|} in
  let prog2 = {|
def foo(x: str | None) -> None: pass
|} in
  ast_diff_equal ~require_type_comparisons:true prog1 prog2 ;
  [%expect
    {| (Line 2) - def foo(x: Optional[int]) -> None: pass, + def foo(x: str | None) -> None: pass |}]


let%expect_test "test_change_any_type_good" =
  let prog1 = {|
def foo(x: Any) -> None: pass
|} in
  let prog2 = {|
def foo(x: object) -> None: pass
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {||}]


let%expect_test "test_change_any_type_bad" =
  let prog1 = {|
def foo(x: Any) -> None: pass
|} in
  let prog2 = {|
def foo(x: str) -> None: pass
|} in
  ast_diff_equal ~require_type_comparisons:true prog1 prog2 ;
  [%expect {| (Line 2) - def foo(x: Any) -> None: pass, + def foo(x: str) -> None: pass |}]


let%expect_test "test_change_type_case_sensitive_rec_good" =
  let prog1 =
    {|
from typing import Dict, Set
def foo() -> Dict[str, Dict[str, Set[str]]]: pass
|}
  in
  let prog2 = {|
def foo() -> dict[str, dict[str, set[str]]]: pass
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {||}]


let%expect_test "test_change_type_case_sensitive_rec_bad" =
  let prog1 =
    {|
from typing import Dict, Set
def foo() -> Dict[str, Dict[str, Set[str]]]: pass
|}
  in
  let prog2 = {|
def foo() -> dict[str, dict[str, str]]: pass
|} in
  ast_diff_equal ~require_type_comparisons:true prog1 prog2 ;
  [%expect
    {|
    (Line 2) + def foo() -> dict[str, dict[str, str]]: pass
    (Line 3) - def foo() -> Dict[str, Dict[str, Set[str]]]: pass
    |}]
