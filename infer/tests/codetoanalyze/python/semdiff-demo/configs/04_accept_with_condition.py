from ast import *
from infer_semdiff import var, ignore, rewrite, accept, null

X = var("X")
T1 = var("T1")
T2 = var("T2")

# Accept new annotations, but REJECT "Any" as a new annotation.
# The condition= uses equals() to check if X matches the AST node
# for the name "Any", and `not` negates it: only accept if X != Any.
accept(
    lhs=null,
    rhs=X,
    condition=not equals(Name(ctx=Load(), id="Any"), X),
    key=["returns", "annotation"]
)

# When an annotation already existed (T1 is not null), accept the new
# annotation T2 as long as T2 does not contain "Any" anywhere in its
# subtree. The contains() predicate searches recursively.
accept(
    lhs=T1,
    rhs=T2,
    condition=(not (equals(null, T1))) and (not (contains(Name(ctx=Load(), id="Any"), T2))),
    key=["returns", "annotation"]
)
