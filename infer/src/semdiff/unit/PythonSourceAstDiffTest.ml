(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module CC = CongruenceClosureSolver
module Rewrite = CongruenceClosureRewrite

let build_parser () =
  let parser = PythonSourceAst.build_parser () in
  fun string -> parser string |> Result.ok |> Option.value_exn


let st = ref (CC.init ~debug:false)

let restart () = st := CC.init ~debug:false

let parse_rule str = Rewrite.parse_rule !st str |> Result.ok |> Option.value_exn

let pp_rules fmt rules =
  F.fprintf fmt "@[<hv1>{" ;
  List.iteri rules ~f:(fun i rule ->
      if i > 0 then F.fprintf fmt "@ " ;
      Rewrite.Rule.pp fmt rule ) ;
  F.fprintf fmt "}@]"


let are_ast_equivalent ?(debug = false) ast1 ast2 rules =
  if debug then (
    PythonSourceAstDiff.TestOnly.store_ast ~debug:true ast1 ;
    F.printf "@." ;
    PythonSourceAstDiff.TestOnly.store_ast ~debug:true ast2 ) ;
  PythonSourceAstDiff.TestOnly.are_ast_equivalent !st ast1 ast2 rules


let%expect_test "store ast" =
  let parser = build_parser () in
  let ast = parser {|
x = 0
y = 1
z = 2
      |} in
  F.printf "%s\n" (PythonSourceAst.Node.to_str ast) ;
  PythonSourceAstDiff.TestOnly.store_ast ~debug:true ast ;
  [%expect
    {|
    Dict: {
      _type=Str: Module
      body=List: [
        Dict: {
          _type=Str: Assign
          end_lineno=Int: 2
          lineno=Int: 2
          targets=List: [
            Dict: {
              _type=Str: Name
              ctx=Dict: {
                _type=Str: Store
              }
              end_lineno=Int: 2
              id=Str: x
              lineno=Int: 2
            }
          ]
          type_comment=Null
          value=Dict: {
            _type=Str: Constant
            end_lineno=Int: 2
            kind=Null
            lineno=Int: 2
            value=Int: 0
          }
        }
        Dict: {
          _type=Str: Assign
          end_lineno=Int: 3
          lineno=Int: 3
          targets=List: [
            Dict: {
              _type=Str: Name
              ctx=Dict: {
                _type=Str: Store
              }
              end_lineno=Int: 3
              id=Str: y
              lineno=Int: 3
            }
          ]
          type_comment=Null
          value=Dict: {
            _type=Str: Constant
            end_lineno=Int: 3
            kind=Null
            lineno=Int: 3
            value=Int: 1
          }
        }
        Dict: {
          _type=Str: Assign
          end_lineno=Int: 4
          lineno=Int: 4
          targets=List: [
            Dict: {
              _type=Str: Name
              ctx=Dict: {
                _type=Str: Store
              }
              end_lineno=Int: 4
              id=Str: z
              lineno=Int: 4
            }
          ]
          type_comment=Null
          value=Dict: {
            _type=Str: Constant
            end_lineno=Int: 4
            kind=Null
            lineno=Int: 4
            value=Int: 2
          }
        }
      ]
      type_ignores=List: [
      ]
    }

    (Module
        (body
            (List
                (Assign
                    (targets (List (Name (ctx Store) (id x))))
                    (type_comment Null)
                    (value (Constant (kind Null) (value 0))))
                (Assign
                    (targets (List (Name (ctx Store) (id y))))
                    (type_comment Null)
                    (value (Constant (kind Null) (value 1))))
                (Assign
                    (targets (List (Name (ctx Store) (id z))))
                    (type_comment Null)
                    (value (Constant (kind Null) (value 2))))))
        (type_ignores List))
    |}]


let%expect_test "compare same ast" =
  let parser = build_parser () in
  restart () ;
  let ast =
    parser
      {|
def factorial(n):
    """
    Returns the factorial of a non-negative integer n.
    Raises ValueError for negative inputs.
    """
    if n < 0:
        raise ValueError("Factorial is not defined for negative numbers.")
    result = 1
    for i in range(2, n + 1):
        result *= i
    return result
      |}
  in
  let equiv = are_ast_equivalent ast ast [] in
  F.printf "ast == ast? %b\n" equiv ;
  [%expect {| ast == ast? true |}]


let%expect_test "ignore_me() call" =
  let parser = build_parser () in
  restart () ;
  let ast =
    parser
      {|
def factorial(n):
    """
    Returns the factorial of a non-negative integer n.
    Raises ValueError for negative inputs.
    """
    if n < 0:
        raise ValueError("Factorial is not defined for negative numbers.")
    result = 1
    for i in range(2, n + 1):
        result *= i
    return result
      |}
  in
  let ast_with_ignore =
    parser
      {|
def factorial(n):
    """
    Returns the factorial of a non-negative integer n.
    Raises ValueError for negative inputs.
    """
    if n < 0:
        raise ValueError("Factorial is not defined for negative numbers.")
    result = 1
    ignore_me()
    for i in range(2, n + 1):
        result *= i
    return result
      |}
  in
  let equiv = are_ast_equivalent ast ast_with_ignore [] in
  F.printf "ast == ast_with_ignore? %b (no rules)\n" equiv ;
  let rules : Rewrite.Rule.t list =
    [ parse_rule "(List ... Null ...) ==> (List ...)"
    ; parse_rule
        {|
              (Expr
                (value
                  (Call
                    (args List)
                    (func (Name (ctx Load) (id ignore_me)))
                    (keywords List)))) ==> Null |}
    ]
  in
  let equiv = are_ast_equivalent ast ast_with_ignore rules in
  F.printf "ast == ast_with_ignore? %b@." equiv ;
  F.printf " with rules: %a@." pp_rules rules ;
  [%expect
    {|
    ast == ast_with_ignore? false (no rules)
    ast == ast_with_ignore? true
     with rules: {(List ... Null ...) ==> (List ...)
                  (Expr
                      (value
                          (Call (args List) (func (Name (ctx Load) (id ignore_me))) (keywords List))))
                  ==>
                  Null}
    |}]


let%expect_test "ignore imports" =
  let parser = build_parser () in
  restart () ;
  let ast1 = parser {|
def greet(name):
    return f"Hello, {name}!"
|} in
  let ast2 =
    parser
      {|
# pyre-strict
from typing import Any
import urllib.parse

def greet(name):
    return f"Hello, {name}!"
|}
  in
  let rules : Rewrite.Rule.t list =
    [ parse_rule "(List ... Null ...) ==> (List ...)"
    ; parse_rule "(ImportFrom (level ?L) (module typing) (names ?N)) ==> Null"
    ; parse_rule "(Import (names ?N)) ==> Null" ]
  in
  let equiv = are_ast_equivalent ast1 ast2 rules in
  F.printf "ast1 == ast2? %b@." equiv ;
  [%expect {| ast1 == ast2? true |}]


let%expect_test "type test modernization" =
  let parser = build_parser () in
  restart () ;
  let ast1 = parser {|
def foo(self, x):
        if x.__class__ == str:
            print(1)
|} in
  let ast2 =
    parser {|
def foo(self, x) -> None:
        if isinstance(x, str):
            print(1)
|}
  in
  let rules : Rewrite.Rule.t list =
    [ parse_rule
        {|        (If
                      (body ?BODY) (orelse ?LIST) (test
                                                      (Compare
                                                          (comparators
                                                              (List (Name (ctx Load) (id str))))
                                                          (left
                                                              (Attribute
                                                                  (attr __class__)
                                                                  (ctx Load)
                                                                  (value ?NAME)))
                                                          (ops (List Eq)))))
                  ==>
                  (If
                      (body ?BODY) (orelse ?LIST) (test
                                                      (Call
                                                          (args
                                                              (List ?NAME (Name (ctx Load) (id str))))
                                                          (func
                                                              (Name (ctx Load) (id isinstance)))
                                                          (keywords
                                                              List))))|}
    ; parse_rule "(returns (Constant (kind Null) (value Null))) ==> (returns Null)" ]
  in
  let equiv = are_ast_equivalent ast1 ast2 rules in
  F.printf "ast1 == ast2? %b@." equiv ;
  [%expect {| ast1 == ast2? true |}]


let%expect_test "remove types" =
  let parser = build_parser () in
  restart () ;
  let ast1 = parser {|
def foo(self, x):
      y = x + 1
      return x
|} in
  let ast2 = parser {|
def foo(self, x: int) -> int:
      y: int = x + 1
      return x
|} in
  let rules : Rewrite.Rule.t list =
    [ parse_rule "(returns ?X) ==> (returns Null)"
    ; parse_rule "(annotation ?X) ==> (annotation Null)"
    ; parse_rule
        {|(AnnAssign (annotation ?A) (simple ?S) (target ?N) (value ?V))
          ==>
          (Assign (targets (List ?N)) (type_comment Null) (value ?V))|}
    ]
  in
  let equiv = are_ast_equivalent ast1 ast2 rules in
  F.printf "ast1 == ast2? %b@." equiv ;
  [%expect {| ast1 == ast2? true |}]


module Diff = PythonSourceAstDiff

let%expect_test "build diff rules" =
  let parser = build_parser () in
  restart () ;
  let ast1 = parser {|
def foo(self, x: Any) -> int:
      y = x + 1
      return x
|} in
  let ast2 = parser {|
def foo(self, x: int) -> int:
      y: int = x + 1
      return x
|} in
  Diff.build_diff !st ast1 ast2 ;
  Diff.TestOnly.gen_diff_rules !st
  |> List.iteri ~f:(fun i rule -> F.printf "RULE%d: %a@." i Rewrite.Rule.pp rule) ;
  [%expect
    {|
    RULE0: (@DIFF ?X ?X) ==> __DONE__
    RULE1: (@DIFF (annotation ?X0) (annotation ?Y0)) ==> (annotation (@DIFF ?X0 ?Y0))
    RULE2: (@DIFF (arg ?X0) (arg ?Y0)) ==> (arg (@DIFF ?X0 ?Y0))
    RULE3: (@DIFF (arg ?X0 ?X1 ?X2) (arg ?Y0 ?Y1 ?Y2))
           ==>
           (arg (@DIFF ?X0 ?Y0) (@DIFF ?X1 ?Y1) (@DIFF ?X2 ?Y2))
    RULE4: (@DIFF (type_comment ?X0) (type_comment ?Y0)) ==> (type_comment (@DIFF ?X0 ?Y0))
    RULE5: (@DIFF (ctx ?X0) (ctx ?Y0)) ==> (ctx (@DIFF ?X0 ?Y0))
    RULE6: (@DIFF (id ?X0) (id ?Y0)) ==> (id (@DIFF ?X0 ?Y0))
    RULE7: (@DIFF (Name ?X0 ?X1) (Name ?Y0 ?Y1)) ==> (Name (@DIFF ?X0 ?Y0) (@DIFF ?X1 ?Y1))
    RULE8: (@DIFF (List ?X0) (List ?Y0)) ==> (List (@DIFF ?X0 ?Y0))
    RULE9: (@DIFF (List ?X0 ?X1) (List ?Y0 ?Y1)) ==> (List (@DIFF ?X0 ?Y0) (@DIFF ?X1 ?Y1))
    RULE10: (@DIFF (args ?X0) (args ?Y0)) ==> (args (@DIFF ?X0 ?Y0))
    RULE11: (@DIFF (defaults ?X0) (defaults ?Y0)) ==> (defaults (@DIFF ?X0 ?Y0))
    RULE12: (@DIFF (kw_defaults ?X0) (kw_defaults ?Y0)) ==> (kw_defaults (@DIFF ?X0 ?Y0))
    RULE13: (@DIFF (kwarg ?X0) (kwarg ?Y0)) ==> (kwarg (@DIFF ?X0 ?Y0))
    RULE14: (@DIFF (kwonlyargs ?X0) (kwonlyargs ?Y0)) ==> (kwonlyargs (@DIFF ?X0 ?Y0))
    RULE15: (@DIFF (posonlyargs ?X0) (posonlyargs ?Y0)) ==> (posonlyargs (@DIFF ?X0 ?Y0))
    RULE16: (@DIFF (vararg ?X0) (vararg ?Y0)) ==> (vararg (@DIFF ?X0 ?Y0))
    RULE17: (@DIFF (arguments ?X0 ?X1 ?X2 ?X3 ?X4 ?X5 ?X6) (arguments ?Y0 ?Y1 ?Y2 ?Y3 ?Y4 ?Y5 ?Y6))
            ==>
            (arguments
                (@DIFF ?X0 ?Y0) (@DIFF ?X1 ?Y1) (@DIFF ?X2 ?Y2) (@DIFF ?X3 ?Y3)
                (@DIFF ?X4 ?Y4) (@DIFF ?X5 ?Y5) (@DIFF ?X6 ?Y6))
    RULE18: (@DIFF (targets ?X0) (targets ?Y0)) ==> (targets (@DIFF ?X0 ?Y0))
    RULE19: (@DIFF (left ?X0) (left ?Y0)) ==> (left (@DIFF ?X0 ?Y0))
    RULE20: (@DIFF (op ?X0) (op ?Y0)) ==> (op (@DIFF ?X0 ?Y0))
    RULE21: (@DIFF (kind ?X0) (kind ?Y0)) ==> (kind (@DIFF ?X0 ?Y0))
    RULE22: (@DIFF (value ?X0) (value ?Y0)) ==> (value (@DIFF ?X0 ?Y0))
    RULE23: (@DIFF (Constant ?X0 ?X1) (Constant ?Y0 ?Y1))
            ==>
            (Constant (@DIFF ?X0 ?Y0) (@DIFF ?X1 ?Y1))
    RULE24: (@DIFF (right ?X0) (right ?Y0)) ==> (right (@DIFF ?X0 ?Y0))
    RULE25: (@DIFF (BinOp ?X0 ?X1 ?X2) (BinOp ?Y0 ?Y1 ?Y2))
            ==>
            (BinOp (@DIFF ?X0 ?Y0) (@DIFF ?X1 ?Y1) (@DIFF ?X2 ?Y2))
    RULE26: (@DIFF (Assign ?X0 ?X1 ?X2) (Assign ?Y0 ?Y1 ?Y2))
            ==>
            (Assign (@DIFF ?X0 ?Y0) (@DIFF ?X1 ?Y1) (@DIFF ?X2 ?Y2))
    RULE27: (@DIFF (Return ?X0) (Return ?Y0)) ==> (Return (@DIFF ?X0 ?Y0))
    RULE28: (@DIFF (body ?X0) (body ?Y0)) ==> (body (@DIFF ?X0 ?Y0))
    RULE29: (@DIFF (decorator_list ?X0) (decorator_list ?Y0)) ==> (decorator_list (@DIFF ?X0 ?Y0))
    RULE30: (@DIFF (name ?X0) (name ?Y0)) ==> (name (@DIFF ?X0 ?Y0))
    RULE31: (@DIFF (returns ?X0) (returns ?Y0)) ==> (returns (@DIFF ?X0 ?Y0))
    RULE32: (@DIFF (FunctionDef ?X0 ?X1 ?X2 ?X3 ?X4 ?X5) (FunctionDef ?Y0 ?Y1 ?Y2 ?Y3 ?Y4 ?Y5))
            ==>
            (FunctionDef
                (@DIFF ?X0 ?Y0) (@DIFF ?X1 ?Y1) (@DIFF ?X2 ?Y2) (@DIFF ?X3 ?Y3)
                (@DIFF ?X4 ?Y4) (@DIFF ?X5 ?Y5))
    RULE33: (@DIFF (type_ignores ?X0) (type_ignores ?Y0)) ==> (type_ignores (@DIFF ?X0 ?Y0))
    RULE34: (@DIFF (Module ?X0 ?X1) (Module ?Y0 ?Y1)) ==> (Module (@DIFF ?X0 ?Y0) (@DIFF ?X1 ?Y1))
    RULE35: (@DIFF (simple ?X0) (simple ?Y0)) ==> (simple (@DIFF ?X0 ?Y0))
    RULE36: (@DIFF (target ?X0) (target ?Y0)) ==> (target (@DIFF ?X0 ?Y0))
    RULE37: (@DIFF (AnnAssign ?X0 ?X1 ?X2 ?X3) (AnnAssign ?Y0 ?Y1 ?Y2 ?Y3))
            ==>
            (AnnAssign (@DIFF ?X0 ?Y0) (@DIFF ?X1 ?Y1) (@DIFF ?X2 ?Y2) (@DIFF ?X3 ?Y3))
    |}]


let%expect_test "build diffs and simplify with generated diff rules" =
  let parser = build_parser () in
  restart () ;
  let ast1 = parser {|
def foo(self, x: Any) -> int:
      y = x + 1
      return x
|} in
  let ast2 = parser {|
def foo(self, x: int) -> int:
      y: int = x + 1
      return x
|} in
  Diff.build_diff !st ast1 ast2 ;
  let rules = Diff.TestOnly.gen_diff_rules !st in
  Rewrite.Rule.full_rewrite !st rules |> ignore ;
  Diff.get_unresolved_diffs !st
  |> List.iter ~f:(fun (left, right) ->
         F.printf "@[<hv 4>(DIFF@ %a@ %a)@]@." (CC.pp_nested_term !st) left (CC.pp_nested_term !st)
           right ) ;
  [%expect
    {|
    (DIFF Any int)
    (DIFF
        (Assign
            (targets (List (Name (ctx Store) (id y))))
            (type_comment Null)
            (value
                (BinOp
                    (left (Name (ctx Load) (id x)))
                    (op Add)
                    (right (Constant (kind Null) (value 1))))))
        (AnnAssign
            (annotation (Name (ctx Load) (id int)))
            (simple 1)
            (target (Name (ctx Store) (id y)))
            (value
                (BinOp
                    (left (Name (ctx Load) (id x)))
                    (op Add)
                    (right (Constant (kind Null) (value 1)))))))
    |}]


let%expect_test "build diffs and simplify with all rules" =
  let parser = build_parser () in
  restart () ;
  let ast1 = parser {|
def foo(self, x: Any) -> int:
      y = x + 1
      return x
|} in
  let ast2 = parser {|
def foo(self, x: int) -> int:
      y: int = x + 1
      return x
|} in
  Diff.build_diff !st ast1 ast2 ;
  let rules = Diff.TestOnly.gen_all_rules !st in
  Rewrite.Rule.full_rewrite !st rules |> ignore ;
  Diff.get_unresolved_diffs !st
  |> List.iter ~f:(fun (left, right) ->
         F.printf "@[<hv 4>(DIFF@ %a@ %a)@]@." (CC.pp_nested_term !st) left (CC.pp_nested_term !st)
           right ) ;
  [%expect {| |}]
