(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This file runs the same test cases as PythonCompareDirectRewriteTest.ml but
   using the equality saturation (eqsat) engine instead of the direct rewrite
   engine. The goal is to track where the two engines agree and disagree.

   Compared to the direct engine, the eqsat engine:
   - Agrees on most cases (imports, basic type annotations, isinstance rewrite)
   - Is more permissive with Any: treats unannotated->Any as equivalent
     (test_with_missing_type_bad, test_change_any_type_bad)
   - Is less permissive on type-to-type changes: the direct engine has a blanket
     "accept any type change in annotation/returns keys" rule that eqsat lacks
     (test_change_fun_type_ok, test_change_async_fun_type_ok, test_change_assign_type_ok)
   - Missing rules:
     * Optional[T] <-> T | None rewrite
     * Assign -> AnnAssign for field assignments (test_field_assign_type_good)
     * Quoted type annotations: "Tree" <-> Tree (type_annotation_with_quotes_good)
     * User-defined type renames (test_change_type_case_sensitive_ok)
     * {} <-> set() structural equivalence (fp_test_initialisation_set_good)
     * Variable extraction refactoring (fp_equivalent_logic_good)
*)

open! IStd
module F = Format

let () = if not (Py.is_initialized ()) then Py.initialize ~interpreter:Version.python_exe ()

let check_equiv ?(debug = false) prog1 prog2 =
  let parse = PythonSourceAst.build_parser () in
  match (parse prog1, parse prog2) with
  | Error error, _ | Ok _, Error error ->
      F.printf "PARSE ERROR: %a@." PythonSourceAst.pp_error error
  | Ok ast1, Ok ast2 ->
      let equiv = PythonSourceAstDiff.check_equivalence ~debug ast1 ast2 in
      F.printf "%s@." (if equiv then "equivalent" else "different")


let%expect_test "test_basic_fun_good" =
  check_equiv "def f():\n  return 1" "def f():\n  return 1" ;
  [%expect {| equivalent |}]


let%expect_test "test_basic_fun_bad" =
  check_equiv "def f():\n  return 1" "def f():\n  return 2" ;
  [%expect {| different |}]


let%expect_test "test_const_type_annot_good" =
  check_equiv {|
# pyre-unsafe

CATEGORIES_TO_REMOVE = {'a': 1, 'b': 2, 'c': 3}
|}
    {|
# pyre-strict

CATEGORIES_TO_REMOVE: dict[str, int | None] = {'a': 1, 'b': 2, 'c': 3}
|} ;
  [%expect {| equivalent |}]


let%expect_test "test_const_type_annot_bad" =
  check_equiv {|
# pyre-unsafe

CATEGORIES_TO_REMOVE = {'a': 1, 'b': 2, 'c': 3}
|}
    {|
# pyre-strict

CATEGORIES_TO_REMOVE_RENAMED: dict[str, int | None] = {'a': 1, 'b': 2, 'c': 3}
|} ;
  [%expect {| different |}]


let%expect_test "test_fun_type_annot_good" =
  check_equiv
    {|
def foo():
    write_html(
        "file.json"
    )

def write_html(json_file_path): pass
|}
    {|
# pyre-strict

def foo() -> None:
    write_html(
        "file.json"
    )

def write_html(json_file_path:str) -> None: pass
|} ;
  [%expect {| equivalent |}]


let%expect_test "test_fun_type_annot_bad" =
  check_equiv
    {|
def foo():
    write_html(
        "file.json"
    )

def write_html(json_file_path): pass
|}
    {|
# pyre-strict

def foo() -> None:
    write_html(
        "file_NEW.json"
    )

def write_html(json_file_path:str) -> None: pass
|} ;
  [%expect {| different |}]


let%expect_test "test_fun_with_assert_bad" =
  check_equiv
    {|
def foo(self) -> None:
    obj = self.obj
    x = obj.prop
|}
    {|
def foo(self) -> None:
    obj = self.obj
    assert obj is not None
    x = obj.prop
|} ;
  [%expect {| different |}]


(* Disagrees with DirectRewrite: eqsat says equivalent because the rule
   "(@DIFF (annotation Null) (annotation ?X)) ==> __DONE__" treats adding
   Any annotation the same as adding any other annotation. DirectRewrite
   explicitly rejects Any annotations via a condition. *)
let%expect_test "test_with_missing_type_bad" =
  check_equiv
    {|
def greet(name):
    return f"Hello, {name}!"
|}
    {|
# pyre-strict
from typing import Any

def greet(name: Any) -> str:
    return f"Hello, {name}!"
|} ;
  [%expect {| equivalent |}]


let%expect_test "test_with_missing_type_good" =
  check_equiv
    {|
def greet(name):
    return f"Hello, {name}!"
|}
    {|
# pyre-strict
def greet(name: str) -> str:
    return f"Hello, {name}!"
|} ;
  [%expect {| equivalent |}]


let%expect_test "test_with_import_dir_good" =
  check_equiv
    {|
def main():
    print("Hello World!")
|}
    {|
import urllib.parse
def main():
    print("Hello World!")
|} ;
  [%expect {| equivalent |}]


let%expect_test "test_import_dir_alias_good" =
  check_equiv
    {|
def main():
    print("Hello World!")
|}
    {|
import urllib.parse as parse
def main():
    print("Hello World!")
|} ;
  [%expect {| equivalent |}]


let%expect_test "test_import_from_dir_good" =
  check_equiv
    {|
def main():
    print("Hello World!")
|}
    {|
from urllib.parse import quote
def main():
    print("Hello World!")
|} ;
  [%expect {| equivalent |}]


let%expect_test "test_import_from_dir_alias_good" =
  check_equiv
    {|
def main():
    print("Hello World!")
|}
    {|
from urllib.parse import quote as q
def main():
    print("Hello World!")
|} ;
  [%expect {| equivalent |}]


let%expect_test "test_import_from_dir_alias_bad" =
  check_equiv
    {|
def main():
    print("Hello World!")
|}
    {|
from urllib.parse import quote as q
def main():
    print(1)
    print("Hello World!")
|} ;
  [%expect {| different |}]


let%expect_test "fn_test_with_import_bad" =
  check_equiv
    {|
def greet(name: str) -> str:
    return f"Hello, {name}!"
print(greet("Alice"))
print(greet.__annotations__)
|}
    {|
from __future__ import annotations
def greet(name: str) -> str:
    return f"Hello, {name}!"
print(greet("Alice"))
print(greet.__annotations__)
|} ;
  [%expect {| equivalent |}]


let%expect_test "test_change_async_fun_param_type_good" =
  check_equiv {|
# pyre-unsafe

async def foo(
  a: bool, b: bool, c
) -> bool:
  return True
|}
    {|
# pyre-strict

async def foo(
  a: bool, b: bool, c: bool
) -> bool:
  return True
|} ;
  [%expect {| equivalent |}]


let%expect_test "test_change_lambda_param_type_good" =
  check_equiv {|
square = lambda x: x * x
print(square(5))
|}
    {|
from typing import Callable

square: Callable[[int], int] = lambda x: x * x
print(square(5))
|} ;
  [%expect {| equivalent |}]


let%expect_test "test_change_async_fun_body_bad" =
  check_equiv
    {|
async def authenticate(self, token: str, tag: str) -> None:
        print(1)
|}
    {|
async def authenticate(self, token: str, tag: str) -> None:
        print(2)
|} ;
  [%expect {| different |}]


let%expect_test "test_change_class_to_is_instance_good" =
  check_equiv
    {|
def foo(self, x):
        if x.__class__ == str:
            print(1)
|}
    {|
def foo(self, x) -> None:
        if isinstance(x, str):
            print(1)
|} ;
  [%expect {| equivalent |}]


let%expect_test "test_change_class_to_is_instance_bad" =
  check_equiv
    {|
def foo(self, x):
        if x.__class__ == str:
            print(1)
|}
    {|
def foo(self, x) -> None:
        if isinstance(x, None):
            print(1)
|} ;
  [%expect {| different |}]


let%expect_test "test_change_async_body_indentation_bad" =
  check_equiv
    {|
async def foo(self, x):
    if x == 1:
        print(1)
        print(2)
|}
    {|
async def foo(self, x):
    if x == 1:
        print(1)
    print(2)
|} ;
  [%expect {| different |}]


let%expect_test "test_change_async_def_kwargs_good" =
  check_equiv {|
async def foo(self, **kwargs): pass
|}
    {|
async def foo(self, **kwargs: int): pass
|} ;
  [%expect {| equivalent |}]


(* Disagrees with DirectRewrite: eqsat has no blanket "accept any type->type
   change in annotation keys" rule, so changing int to int|None is a diff. *)
let%expect_test "test_change_fun_type_ok" =
  check_equiv {|
def foo(self, x: int) -> None: pass
|}
    {|
def foo(self, x: int | None) -> None: pass
|} ;
  [%expect {| different |}]


(* Disagrees with DirectRewrite: same reason — no blanket type-change accept rule. *)
let%expect_test "test_change_async_fun_type_ok" =
  check_equiv {|
async def foo(self, x: int) -> None: pass
|}
    {|
async def foo(self, x: str) -> None: pass
|} ;
  [%expect {| different |}]


(* Disagrees with DirectRewrite: same reason — no blanket type-change accept rule. *)
let%expect_test "test_change_assign_type_ok" =
  check_equiv {|
# pyre-unsafe

CATEGORIES_TO_REMOVE: dict[str, int] = {'a': 1, 'b': 2, 'c': 3}
|}
    {|
# pyre-strict

CATEGORIES_TO_REMOVE: dict[str, int | None] = {'a': 1, 'b': 2, 'c': 3}
|} ;
  [%expect {| different |}]


let%expect_test "test_change_fun_type_float_good" =
  check_equiv {|
def foo(x) -> None: pass
|} {|
def foo(x:float) -> None: pass
|} ;
  [%expect {| equivalent |}]


let%expect_test "test_change_fun_type_ellipsis_good" =
  check_equiv {|
def foo(f) -> None: pass
|}
    {|
from typing import Callable

def foo(f: Callable[..., int]) -> None: pass
|} ;
  [%expect {| equivalent |}]


let%expect_test "test_change_type_case_sensitive_ret_good" =
  check_equiv {|
from typing import Dict
def foo() -> Dict[str, str]: pass
|}
    {|
def foo() -> dict[str, str]: pass
|} ;
  [%expect {| equivalent |}]


let%expect_test "test_change_type_case_sensitive_param_good" =
  check_equiv {|
from typing import Dict
def foo(x: Dict[str, str]) -> None: pass
|}
    {|
def foo(x: dict[str, str]) -> None: pass
|} ;
  [%expect {| equivalent |}]


(* Disagrees with DirectRewrite: eqsat has no rule for user-defined type renames.
   DirectRewrite accepts this because imports are ignored and the type change is
   covered by the blanket accept rule on returns/annotation keys. *)
let%expect_test "test_change_type_case_sensitive_ok" =
  check_equiv {|
from mylib import SomeUserDefinedType
def foo() -> SomeUserDefinedType: pass
|}
    {|
from mylib import someUserDefinedType
def foo() -> someUserDefinedType: pass
|} ;
  [%expect {| different |}]


(* Disagrees with DirectRewrite (which also marks this as FP): {} and set()
   are structurally different ASTs. Neither engine handles this well. *)
let%expect_test "fp_test_initialisation_set_good" =
  check_equiv {|
self.errors = {}
  |} {|
self.errors: set[str] = set()
  |} ;
  [%expect {| different |}]


(* Disagrees with DirectRewrite (which also marks this as FP): variable
   extraction changes the AST structure in ways neither engine can reconcile. *)
let%expect_test "fp_equivalent_logic_good" =
  check_equiv
    {|
def foo():
  if self.field and self.field.label != TOP:
    return None
  return 0
  |}
    {|
def foo():
  field = self.field
  if field and field.label != TOP:
    return None
  return 0
  |} ;
  [%expect {| different |}]


(* Disagrees with DirectRewrite: eqsat has no rule for quoted type annotations
   ("Tree" as a Constant string vs Tree as a Name node). *)
let%expect_test "type_annotation_with_quotes_good" =
  check_equiv {|
def foo(params) -> "Tree": pass
|} {|
def foo(params: list[str]) -> Tree: pass
|} ;
  [%expect {| different |}]


let%expect_test "noise_in_diff_bad" =
  check_equiv {|
def foo() -> None:
  x = 1
  y = 2
  z = 3
  d = 4
  e = 5
|}
    {|
def foo() -> None:
  r = 2
  x = 1
  y = 2
  z = 3
  d = 4
  e = 5
|} ;
  [%expect {| different |}]


(* Disagrees with DirectRewrite: eqsat has no Optional[T] <-> T|None rewrite
   rule. Adding one (Subscript(Optional,T) ==> BinOp(T,BitOr,None)) would fix
   this case. *)
let%expect_test "test_change_optional_type_good" =
  check_equiv {|
def foo(x: Optional[int]) -> None: pass
|}
    {|
def foo(x: int | None) -> None: pass
|} ;
  [%expect {| different |}]


(* Disagrees with DirectRewrite: same missing Optional rule, plus even with it,
   Optional[int] rewrites to int|None which differs from str|None. DirectRewrite
   accepts this via its blanket type-change accept rule. *)
let%expect_test "test_change_optional_type_ok" =
  check_equiv {|
def foo(x: Optional[int]) -> None: pass
|}
    {|
def foo(x: str | None) -> None: pass
|} ;
  [%expect {| different |}]


let%expect_test "test_change_any_type_good" =
  check_equiv {|
def foo(x: Any) -> None: pass
|} {|
def foo(x: object) -> None: pass
|} ;
  [%expect {| equivalent |}]


(* Disagrees with DirectRewrite: eqsat treats unannotated->Any as equivalent
   because "(@DIFF (annotation Null) (annotation ?X)) ==> __DONE__" matches
   without checking what ?X is. DirectRewrite rejects Any via a condition:
   not(equals(Name(id="Any"), X)). *)
let%expect_test "test_change_any_type_bad" =
  check_equiv {|
def foo(x) -> None: pass
|} {|
def foo(x: Any) -> None: pass
|} ;
  [%expect {| equivalent |}]


let%expect_test "test_change_any_type_ok" =
  check_equiv {|
def foo(x: Any) -> None: pass
|} {|
def foo(x: str) -> None: pass
|} ;
  [%expect {| equivalent |}]


let%expect_test "test_change_type_case_sensitive_rec_good" =
  check_equiv {|
from typing import Dict, Set
def foo() -> Dict[str, Dict[str, Set[str]]]: pass
|}
    {|
def foo() -> dict[str, dict[str, set[str]]]: pass
|} ;
  [%expect {| equivalent |}]


let%expect_test "test_change_type_case_sensitive_rec_bad" =
  check_equiv {|
from typing import Dict, Set
def foo() -> Dict[str, Dict[str, Set[str]]]: pass
|}
    {|
def foo() -> dict[str, dict[str, Any]]: pass
|} ;
  [%expect {| different |}]


(* Disagrees with DirectRewrite: the Assign->AnnAssign rewrite rule exists in
   eqsat but does not fire here because the target is an Attribute node
   (self.msg) rather than a simple Name, and the "simple" field differs (1 vs 0
   for attribute targets). See TODO in gen_all_rules. *)
let%expect_test "test_field_assign_type_good" =
  check_equiv
    {|
def __init__(self):
    self.msg = "hello"
|}
    {|
def __init__(self) -> None:
    self.msg : str = "hello"
|} ;
  [%expect {| different |}]


let%expect_test "for loop bad" =
  check_equiv
    {|
def main():
    for i in range(7, 13):
        o.mapping[i] -= 7
    action1()
    action2()
|}
    {|
def main():
    temp_dict = dict(o.mapping)
    for i in range(7, 13):
        temp_dict[i] -= 7
    o.mapping = temp_dict
    action1()
    action2()
|} ;
  [%expect {| different |}]
