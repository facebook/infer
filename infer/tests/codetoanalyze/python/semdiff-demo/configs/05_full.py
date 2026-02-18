from ast import *
from infer_semdiff import var, ignore, rewrite, accept, null

A = var("A")
L = var("L")
M = var("M")
N = var("N")
T1 = var("T1")
T2 = var("T2")
V = var("V")
X = var("X")

# --- Ignore rules ---
# Filter out all import statements from both sides
ignore(ImportFrom(level=L, module=M, names=N))
ignore(Import(names=N))

# --- Rewrite rules (bidirectional equivalence) ---
# Normalize Assign into AnnAssign
rewrite(
    lhs=Assign(targets=[N], type_comment=null, value=V),
    rhs=AnnAssign(annotation=null, simple=1, target=N, value=V),
)
# Ignore the "simple" flag
rewrite(
    lhs=AnnAssign(annotation=A, simple=0, target=N, value=V),
    rhs=AnnAssign(annotation=A, simple=1, target=N, value=V),
)

# --- Accept rules (unidirectional: previous -> current) ---
# Accept new annotations (except Any) in annotation positions
accept(
    lhs=null,
    rhs=X,
    condition=not equals(Name(ctx=Load(), id="Any"), X),
    key=["returns", "annotation"]
)

# Accept changed annotations as long as the new one doesn't contain Any
accept(
    lhs=T1,
    rhs=T2,
    condition=(not (equals(null, T1))) and (not (contains(Name(ctx=Load(), id="Any"), T2))),
    key=["returns", "annotation"]
)
