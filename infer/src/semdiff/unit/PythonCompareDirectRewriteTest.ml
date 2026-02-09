(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let () = if not (Py.is_initialized ()) then Py.initialize ~interpreter:Version.python_exe ()

let ast_diff_equal ?(debug = false) prog1 prog2 =
  let config = PythonCompareDirectRewrite.missing_python_type_annotations_config in
  let diffs = PythonCompareDirectRewrite.ast_diff ~debug ~config prog1 prog2 in
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
  [%expect {| |}]


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
  [%expect {| |}]


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


let%expect_test "test_with_missing_type_bad" =
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
  [%expect
    {| (Line 5) + def greet(name: Any) -> str: |}]


let%expect_test "test_with_missing_type_good" =
  let prog1 = {|
def greet(name):
    return f"Hello, {name}!"
|} in
  let prog2 = {|
# pyre-strict
def greet(name: str) -> str:
    return f"Hello, {name}!"
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {| |}]


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
  [%expect {| |}]


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
  [%expect {| |}]


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
  [%expect {| |}]


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
  [%expect {| |}]


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
  [%expect {| |}]


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
  [%expect {| |}]


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
  [%expect {| |}]


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
  [%expect {| |}]


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
  [%expect {| |}]


let%expect_test "test_change_fun_type_bad" =
  let prog1 = {|
def foo(self, x: int) -> None: pass
|} in
  let prog2 = {|
def foo(self, x: int | None) -> None: pass
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {| |}]


let%expect_test "test_change_async_fun_type_bad" =
  let prog1 = {|
async def foo(self, x: int) -> None: pass
|} in
  let prog2 = {|
async def foo(self, x: str) -> None: pass
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {| |}]


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
  ast_diff_equal prog1 prog2 ;
  [%expect {| |}]


let%expect_test "test_change_fun_type_float_good" =
  let prog1 = {|
def foo(x) -> None: pass
|} in
  let prog2 = {|
def foo(x:float) -> None: pass
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {| |}]


let%expect_test "test_change_fun_type_ellipsis_good" =
  let prog1 = {|
def foo(f) -> None: pass
|} in
  let prog2 = {|
from typing import Callable

def foo(f: Callable[..., int]) -> None: pass
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {| |}]


let%expect_test "test_change_type_case_sensitive_ret_good" =
  let prog1 = {|
from typing import Dict
def foo() -> Dict[str, str]: pass
|} in
  let prog2 = {|
def foo() -> dict[str, str]: pass
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {| |}]


let%expect_test "test_change_type_case_sensitive_param_good" =
  let prog1 = {|
from typing import Dict
def foo(x: Dict[str, str]) -> None: pass
|} in
  let prog2 = {|
def foo(x: dict[str, str]) -> None: pass
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {| |}]


let%expect_test "test_change_type_case_sensitive_bad" =
  let prog1 = {|
from mylib import SomeUserDefinedType
def foo() -> SomeUserDefinedType: pass
|} in
  let prog2 = {|
from mylib import someUserDefinedType
def foo() -> someUserDefinedType: pass
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {| |}]


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
  ast_diff_equal prog1 prog2 ;
  [%expect {| |}]


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


let%expect_test "test_change_optional_type_good" =
  let prog1 = {|
def foo(x: Optional[int]) -> None: pass
|} in
  let prog2 = {|
def foo(x: int | None) -> None: pass
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {| |}]


let%expect_test "test_change_optional_type_bad" =
  let prog1 = {|
def foo(x: Optional[int]) -> None: pass
|} in
  let prog2 = {|
def foo(x: str | None) -> None: pass
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {| |}]


let%expect_test "test_change_any_type_good" =
  let prog1 = {|
def foo(x: Any) -> None: pass
|} in
  let prog2 = {|
def foo(x: object) -> None: pass
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {| |}]


let%expect_test "test_change_any_type_bad" =
  let prog1 = {|
def foo(x: Any) -> None: pass
|} in
  let prog2 = {|
def foo(x: str) -> None: pass
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {| |}]


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
  [%expect {| |}]


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
  ast_diff_equal prog1 prog2 ;
  [%expect {| |}]


let%expect_test "test_field_assign_type" =
  let prog1 = {|
def __init__(self):
    self.msg = "hello"
|} in
  let prog2 = {|
def __init__(self) -> None:
    self.msg : str = "hello"
|} in
  ast_diff_equal prog1 prog2 ;
  [%expect {| |}]


let%expect_test "for loop bad" =
  let prog1 =
    {|
def main():
    for i in range(7, 13):
        o.mapping[i] -= 7
    action1()
    action2()
|}
  in
  let prog2 =
    {|
def main():
    temp_dict = dict(o.mapping)
    for i in range(7, 13):
        temp_dict[i] -= 7
    o.mapping = temp_dict
    action1()
    action2()
|}
  in
  (*  Diff.debug := true ; *)
  ast_diff_equal prog1 prog2 ;
  [%expect
    {|
    (Line 3) +     temp_dict = dict(o.mapping)
    (Line 4) -         o.mapping[i] -= 7
    (Line 5) +         temp_dict[i] -= 7
    (Line 6) +     o.mapping = temp_dict
    |}]


let%expect_test "pp missing_python_type_annotations_config" =
  let open PythonCompareDirectRewrite in
  F.printf "%a@." Rules.pp missing_python_type_annotations_config ;
  [%expect
    {|
    vars: A C L M N T T1 T2 V X
    ignore(ImportFrom(level=L,module=M,names=N))
    ignore(Import(names=N))

    rewrite(lhs=Assign(targets=[N],type_comment=null,value=V),
            rhs=AnnAssign(annotation=null,simple=1,target=N,value=V))
    rewrite(lhs=AnnAssign(annotation=A,simple=0,target=N,value=V),
            rhs=AnnAssign(annotation=A,simple=1,target=N,value=V))
    rewrite(lhs=Compare(comparators=[Name(ctx=Load(),id="str")],left=Attribute(attr="__class__",ctx=Load(),value=N),ops=[Eq()]),
            rhs=Call(args=[N,Name(ctx=Load(),id="str")],func=Name(ctx=Load(),id="isinstance"),keywords=[]))

    accept(lhs=null,
           rhs=X,
           condition=not (equals(Name(ctx=Load(),id="Any"),X)),
           key=["returns","annotation"])
    accept(lhs=T1, rhs=T2, condition=not (equals(null,T1)), key=["returns","annotation","rule2"])
    accept(lhs=Subscript(value=Name(id="Optional",ctx=Load()),slice=T,ctx=Load()),
           rhs=BinOp(left=T,op=BitOr(),right=Constant(kind=null,value=null)))
    accept(lhs=Name(id="Dict",ctx=C), rhs=Name(id="dict",ctx=C))
    accept(lhs=Name(id="FrozenSet",ctx=C), rhs=Name(id="frozenset",ctx=C))
    accept(lhs=Name(id="List",ctx=C), rhs=Name(id="list",ctx=C))
    accept(lhs=Name(id="Tuple",ctx=C), rhs=Name(id="tuple",ctx=C))
    accept(lhs=Name(id="Set",ctx=C), rhs=Name(id="set",ctx=C))
    |}]
