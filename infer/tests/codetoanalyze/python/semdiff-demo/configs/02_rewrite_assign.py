from ast import *
from infer_semdiff import var, ignore, rewrite, accept, null

A = var("A")
N = var("N")
V = var("V")
X = var("X")

# Normalize plain assignment (Assign) into annotated assignment (AnnAssign).
# This is BIDIRECTIONAL: it says the two syntactic forms are equivalent.
# After rewriting, "x = 5" and "x: int = 5" share the same AnnAssign structure.
rewrite(
    lhs=Assign(targets=[N], type_comment=null, value=V),
    rhs=AnnAssign(annotation=null, simple=1, target=N, value=V),
)

# The "simple" flag on AnnAssign is irrelevant for semantic comparison.
rewrite(
    lhs=AnnAssign(annotation=A, simple=0, target=N, value=V),
    rhs=AnnAssign(annotation=A, simple=1, target=N, value=V),
)

# After rewriting, both sides are AnnAssign nodes. But the annotation
# fields still differ (null vs "int"). We need to accept that change,
# restricted to the "annotation" AST field position.
accept(lhs=null, rhs=X, key=["annotation"])
