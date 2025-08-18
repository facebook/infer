(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2

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


let suite =
  "PythonCompareWithoutTypeAnnotTest"
  >::: [ "test_basic_fun_good" >:: test_basic_fun_good
       ; "test_basic_fun_bad" >:: test_basic_fun_bad
       ; "test_const_type_annot_good" >:: test_const_type_annot_good
       ; "test_const_type_annot_bad" >:: test_const_type_annot_bad
       ; "test_fun_type_annot_good" >:: test_fun_type_annot_good
       ; "test_fun_type_annot_bad" >:: test_fun_type_annot_bad
       ; "test_fun_with_assert_bad" >:: test_fun_with_assert_bad ]


let () = run_test_tt_main suite
