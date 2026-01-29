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

# if N.class == str <=> if isinstance(N, str)
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

# if the parent file was not annotated, the new version can be annotated with any type
accept(lhs=null, rhs=X)

# if the parent file was annotated with 'Any', we accept 'object' instead
accept(lhs="Any", rhs="object")

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
  [%expect {| parsing was successful |}]
