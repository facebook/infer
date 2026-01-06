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

let parse_rule str = Rewrite.parse_rule !st str |> Option.value_exn

let pp_rules fmt rules =
  F.fprintf fmt "@[<hv1>{" ;
  List.iteri rules ~f:(fun i rule ->
      if i > 0 then F.fprintf fmt "@ " ;
      Rewrite.Rule.pp fmt rule ) ;
  F.fprintf fmt "}@]"


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
  let equiv = PythonSourceAstDiff.are_ast_equivalent !st ast ast [] in
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
  let equiv = PythonSourceAstDiff.are_ast_equivalent !st ast ast_with_ignore [] in
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
  let equiv = PythonSourceAstDiff.are_ast_equivalent !st ast ast_with_ignore rules in
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
  let equiv = PythonSourceAstDiff.are_ast_equivalent !st ast1 ast2 rules in
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
  let equiv = PythonSourceAstDiff.are_ast_equivalent !st ast1 ast2 rules in
  F.printf "ast1 == ast2? %b@." equiv ;
  [%expect {| ast1 == ast2? true |}]
