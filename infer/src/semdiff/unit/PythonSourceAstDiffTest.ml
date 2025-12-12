(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let%expect_test "" =
  let parser = PythonSourceAst.build_parser () in
  let ast = parser {|
x = 0
y = 1
z = 2
      |} |> Result.ok |> Option.value_exn in
  F.printf "%s\n" (PythonSourceAst.Node.to_str ast) ;
  PythonSourceAstDiff.store_ast ~debug:true ast ;
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
