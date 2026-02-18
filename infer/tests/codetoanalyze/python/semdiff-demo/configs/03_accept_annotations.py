from ast import *
from infer_semdiff import var, ignore, rewrite, accept, null

X = var("X")

# Accept any new type annotation where there was none (null -> X).
# This is UNIDIRECTIONAL: going from null to X is accepted, but going
# from X to null would NOT be.
#
# The key= restricts this rule to AST fields named "returns" or
# "annotation". These are the standard Python AST field names for:
#   - "returns": the return type annotation on a FunctionDef node
#   - "annotation": the type annotation on a function argument
#
# You can discover these names with:
#   python3 -c "import ast; print(ast.dump(ast.parse('def f(x: int) -> str: pass'), indent=2))"
accept(lhs=null, rhs=X, key=["returns", "annotation"])
