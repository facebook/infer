(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PythonCompareDirectRewrite
open PythonConfigParser

let%expect_test "test missing_python_type_annotations_config parser" =
  let rules1 = missing_python_type_annotations_config in
  let rules2 =
    parse_string
      {|
from ast import *
from infer_semdiff import var, ignore, rewrite, accept, null, store

A = var("A")
C = var("C")
L = var("L")
M = var("M")
N = var("N")
T = var("T")
V = var("V")
X = var("X")

# we ignore all import statements
ignore(ImportFrom(level=L, module=M, names=N))
ignore(Import(names=N))

# an unannotated assignment is turned into an annotated one with Null as type annotation
rewrite(
    lhs=Assign(targets=[N], type_comment=null, value=V),
    rhs=AnnAssign(annotation=null, simple=1, target=N, value=V),
)

# annotated statements are simple or not (see documentation), but we don't care
rewrite(
    lhs=AnnAssign(annotation=A, simple=0, target=N, value=V),
    rhs=AnnAssign(annotation=A, simple=1, target=N, value=V),
)

# if N.class == str ==> if isinstance(N, str)
rewrite(
    lhs=Compare(
        comparators=[Name(ctx=Load(), id="str")],
        left=Attribute(attr="__class__", ctx=Load(), value=N),
        ops=[Eq()],
    ),
    rhs=Call(
        args=[N, Name(ctx=Load(), id="str")],
        func=Name(ctx=Load(), id="isinstance"),
        keywords=[],
    ),
)

# if the parent file was not annotated, the new version can be annotated with any type, except Any
accept(lhs=null, rhs=X, condition=not equals(Name(ctx=Load(), id="Any"), X), key=["returns","annotation"])

# if the parent file was annotated', we accept any type as long as it does not contain Any
accept(lhs=T1,
       rhs=T2,
       condition=(not (equals(null,T1))) and (not (contains(Name(ctx=Load(),id="Any"),T2))),
       key=["returns","annotation"])

# if the parent was annotated with Optional[T], we require T | None instead
accept(
    lhs=Subscript(
        value=Name(id="Optional",ctx=Load()),
        slice=T,ctx=Load()
    ),
    rhs=BinOp(
        left=T,
        op=BitOr(),
        right=Constant(kind=null,value=null)
    )
)

# if the parent file was annotated with 'Dict[T]', we require 'dict[T]' instead
accept(lhs=Name(id="Dict", ctx=C), rhs=Name(id="dict", ctx=C))

# if the parent file was annotated with 'FrozenSet[T]', we require 'frozenset[T]' instead
accept(lhs=Name(id="FrozenSet", ctx=C), rhs=Name(id="frozenset", ctx=C))

# if the parent file was annotated with 'List[T]', we require 'list[T]' instead
accept(lhs=Name(id="List", ctx=C), rhs=Name(id="list", ctx=C))

# if the parent file was annotated with 'Tuple[T]', we require 'tuple[T]' instead
accept(lhs=Name(id="Tuple", ctx=C), rhs=Name(id="tuple", ctx=C))

# if the parent file was annotated with 'Set[T]', we require 'set[T]' instead
accept(lhs=Name(id="Set", ctx=C), rhs=Name(id="set", ctx=C))
|}
  in
  if Rules.equal rules1 rules2 then F.printf "parsing was successful" else F.printf "parsing failed" ;
  F.printf "%a@." Rules.pp rules1 ;
  F.printf "%a@." Rules.pp rules2 ;
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  ("IBase.Die.InferUserError(\"line 48, assign\\n\")")
  Raised at SemDiffLib__PythonConfigParser.parse_module.(fun) in file "src/semdiff/PythonConfigParser.ml", line 250, characters 12-95
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from SemDiffLib__PythonConfigParser.parse_module in file "src/semdiff/PythonConfigParser.ml", lines 237-250, characters 4-97
  Called from SemDiffLib__PythonConfigParser.parse_string in file "src/semdiff/PythonConfigParser.ml", line 261, characters 8-24
  Re-raised at IBase__Die.raise_error.do_raise in file "src/base/Die.ml", line 26, characters 8-58
  Called from SemDiffLinTest__PythonConfigParserTest.(fun) in file "src/semdiff/unit/PythonConfigParserTest.ml", lines 16-96, characters 4-2
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28

  Trailing output
  ---------------
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  line 48, assign

  Raised at SemDiffLib__PythonConfigParser.parse_module.(fun) in file "src/semdiff/PythonConfigParser.ml", line 250, characters 12-95
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from SemDiffLib__PythonConfigParser.parse_module in file "src/semdiff/PythonConfigParser.ml", lines 237-250, characters 4-97
  Called from SemDiffLib__PythonConfigParser.parse_string in file "src/semdiff/PythonConfigParser.ml", line 261, characters 8-24
  |}]
