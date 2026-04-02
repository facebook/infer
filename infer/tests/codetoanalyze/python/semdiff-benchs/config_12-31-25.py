# (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

# pyre-ignore-all-errors

from ast import (
    AnnAssign,
    Assign,
    Attribute,
    BinOp,
    BitOr,
    Call,
    Compare,
    Constant,
    Eq,
    Import,
    ImportFrom,
    Load,
    Name,
    Subscript,
)

from infer_semdiff import accept, ignore, null, rewrite, var

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

# if the parent file was annotated with 'Optional[T]', we require 'T|None' instead
accept(
    lhs=Subscript(value=Name(id="Optional", ctx=Load()), slice=T, ctx=Load()),
    rhs=BinOp(left=T, op=BitOr(), right=Constant(kind=null, value=null)),
)
