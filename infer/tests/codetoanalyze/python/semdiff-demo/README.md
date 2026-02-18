# Semdiff Demo

## What is semdiff?

`infer semdiff` is a semantic diff tool for Python programs. Given two versions
of a Python file ("previous" and "current"), it determines whether the changes
are semantically meaningful or merely cosmetic.

This is useful during large-scale migration campaigns (e.g., adding type
annotations, modernizing `typing` imports) where thousands of files are
modified but only the cosmetic, non-functional changes should be accepted
automatically.

The tool is driven by a **configuration file** that defines rules for which
changes to ignore, rewrite, or accept. This demo walks through building such a
configuration file step by step.

## Prerequisites

- `infer` must be in your path 
- Python 3.8+

## Understanding the Python AST

Semdiff configuration files use Python AST node names and field names directly.
The standard `ast.dump()` function is the key tool for understanding what
patterns to write.

For example, to see how Python represents `x = 5`:

```bash
python3 -c "import ast; print(ast.dump(ast.parse('x = 5'), indent=2))"
```

```
Module(
  body=[
    Assign(
      targets=[
        Name(id='x', ctx=Store())],
      value=Constant(value=5))],
  type_ignores=[])
```

And `x: int = 5`:

```bash
python3 -c "import ast; print(ast.dump(ast.parse('x: int = 5'), indent=2))"
```

```
Module(
  body=[
    AnnAssign(
      target=Name(id='x', ctx=Store()),
      annotation=Name(id='int', ctx=Load()),
      value=Constant(value=5),
      simple=1)],
  type_ignores=[])
```

Notice that `x = 5` produces an `Assign` node while `x: int = 5` produces an
`AnnAssign` node with an `annotation` field. These structural differences are
exactly what semdiff rules operate on.

Use `ast.dump()` throughout this demo to understand the AST structure of any
Python snippet. The node names and field names you see in the dump are the same
ones you use in configuration files.

## Configuration file syntax

A semdiff configuration file is a valid Python file with:

```python
from ast import *
from infer_semdiff import var, ignore, rewrite, accept, null
```

It defines **metavariables** (with `var()`) and three kinds of rules:

| Rule | Direction | Semantics |
|------|-----------|-----------|
| `ignore(pattern)` | — | Filter out matching nodes from both sides before comparison |
| `rewrite(lhs=..., rhs=...)` | Bidirectional | Both forms are equivalent; normalizes both sides to the same canonical form |
| `accept(lhs=..., rhs=...)` | Unidirectional | Accept a change from `lhs` (previous) to `rhs` (current), but NOT the reverse |

The `accept` rule also supports optional `key=` and `condition=` parameters,
introduced in Scenarios 3 and 4 respectively.

## Running semdiff

The general command line is:

```bash
infer semdiff \
    --semdiff-configuration <config.py> \
    --semdiff-previous <before.py> \
    --semdiff-current <after.py> \
    -o <output-dir>
```

- `semdiff` — Selects the semdiff subcommand of infer.
- `--semdiff-configuration <config.py>` — Path to the configuration file defining the rules.
- `--semdiff-previous <before.py>` — The original Python file (before changes).
- `--semdiff-current <after.py>` — The modified Python file (after changes).
- `-o <output-dir>` — Directory where results are written. The comparison result is stored in `<output-dir>/semdiff.json`.

---

## Scenario 1: `ignore()` — Ignoring import changes

**Concept:** The simplest rule type. `ignore()` filters out matching AST nodes
from both the previous and current file before comparing them.

To understand the patterns used in the config, let's see how Python represents
import statements:

```bash
python3 -c "import ast; print(ast.dump(ast.parse('import os'), indent=2))"
```

```
Module(
  body=[
    Import(
      names=[
        alias(name='os')])],
  type_ignores=[])
```

```bash
python3 -c "import ast; print(ast.dump(ast.parse('from pathlib import Path'), indent=2))"
```

```
Module(
  body=[
    ImportFrom(
      module='pathlib',
      names=[
        alias(name='Path')],
      level=0)],
  type_ignores=[])
```

`import os` produces an `Import` node with a `names` field, while
`from pathlib import Path` produces an `ImportFrom` node with `module`,
`names`, and `level` fields. The config patterns below match these node
structures exactly.

**Config** (`configs/01_ignore_imports.py`):

```python
from ast import *
from infer_semdiff import var, ignore, rewrite, accept, null

L = var("L")
M = var("M")
N = var("N")

ignore(ImportFrom(level=L, module=M, names=N))
ignore(Import(names=N))
```

The metavariables `L`, `M`, `N` match any AST subtree, so these two rules
match all `import` and `from ... import ...` statements regardless of content.

**Before** (`examples/01_imports/before.py`):

```python
import os

def get_size(path):
    return os.path.getsize(path)
```

**After** (`examples/01_imports/after.py`):

```python
from pathlib import Path

def get_size(path):
    return os.path.getsize(path)
```

**Run:**

```bash
infer semdiff \
    --semdiff-configuration configs/01_ignore_imports.py \
    --semdiff-previous examples/01_imports/before.py \
    --semdiff-current examples/01_imports/after.py \
    -o /tmp/semdiff-out

jq . /tmp/semdiff-out/semdiff.json
```

**Output:**

```json
{
  "previous": "examples/01_imports/before.py",
  "current": "examples/01_imports/after.py",
  "outcome": "equal",
  "diff": []
}
```

The import change is ignored, and the function body is identical.

**Compare with an empty config** (`configs/empty.py`, no rules):

```python
# An empty configuration file with no rules.
# Used to demonstrate what happens when semdiff has no rules:
# any structural difference is reported.
from ast import *
from infer_semdiff import var, ignore, rewrite, accept, null
```

```bash
infer semdiff \
    --semdiff-configuration configs/empty.py \
    --semdiff-previous examples/01_imports/before.py \
    --semdiff-current examples/01_imports/after.py \
    -o /tmp/semdiff-out

jq . /tmp/semdiff-out/semdiff.json
```

**Output:**

```json
{
  "previous": "examples/01_imports/before.py",
  "current": "examples/01_imports/after.py",
  "outcome": "different",
  "diff": [
    "(Line 1) - import os, + from pathlib import Path"
  ]
}
```

Without the `ignore()` rules, the import change is reported as a difference.
The `diff` array uses a unified-diff-like notation: `-` marks lines from the
previous file and `+` marks lines from the current file.

---

## Scenario 2: `rewrite()` — Bidirectional equivalence

**Concept:** `rewrite()` declares that two syntactic forms are equivalent.
Both sides are normalized to the same canonical form before comparison.
This is **bidirectional** — it doesn't matter which direction the change goes.

Looking at the AST dumps above, `x = 5` is an `Assign` node while
`x: int = 5` is an `AnnAssign` node. The rewrite rule normalizes `Assign`
into `AnnAssign` (with a null annotation), making the two structurally
comparable.

**Config** (`configs/02_rewrite_assign.py`):

```python
from ast import *
from infer_semdiff import var, ignore, rewrite, accept, null

A = var("A")
N = var("N")
V = var("V")
X = var("X")

rewrite(
    lhs=Assign(targets=[N], type_comment=null, value=V),
    rhs=AnnAssign(annotation=null, simple=1, target=N, value=V),
)

rewrite(
    lhs=AnnAssign(annotation=A, simple=0, target=N, value=V),
    rhs=AnnAssign(annotation=A, simple=1, target=N, value=V),
)

accept(lhs=null, rhs=X, key=["annotation"])
```

After the rewrite, both sides become `AnnAssign` nodes. But the `annotation`
fields still differ (`null` vs `Name(id='int')`). The `accept` rule handles
this: it accepts a change from `null` to any value `X`. The `key=["annotation"]`
restricts this rule so it only applies to the `annotation` field of AST nodes
(we will explain `key` in detail in Scenario 3).

**Before** (`examples/02_assign_to_annassign/before.py`):

```python
def compute(x):
    result = x * 2
    return result
```

**After** (`examples/02_assign_to_annassign/after.py`):

```python
def compute(x):
    result: int = x * 2
    return result
```

**Run:**

```bash
infer semdiff \
    --semdiff-configuration configs/02_rewrite_assign.py \
    --semdiff-previous examples/02_assign_to_annassign/before.py \
    --semdiff-current examples/02_assign_to_annassign/after.py \
    -o /tmp/semdiff-out

jq . /tmp/semdiff-out/semdiff.json
```

**Output:**

```json
{
  "previous": "examples/02_assign_to_annassign/before.py",
  "current": "examples/02_assign_to_annassign/after.py",
  "outcome": "equal",
  "diff": []
}
```

---

## Scenario 3: `accept()` with `key` — Adding function annotations

**Concept:** `accept()` is **unidirectional**: it accepts a change from `lhs`
(previous) to `rhs` (current), but NOT the reverse. This is the key difference
with `rewrite()`, which is bidirectional. A `rewrite` says "these two forms
are equivalent, no matter which one appears in which file." An `accept` says
"it is OK for the previous file to have `lhs` and the current file to have
`rhs`, but not the other way around." This directionality is useful for
migrations where changes should only go in one direction (e.g., adding
annotations, not removing them).

The `key=` parameter restricts which AST field positions the rule applies to.
But how do you know which field names to use? Use `ast.dump()`:

```bash
python3 -c "import ast; print(ast.dump(ast.parse('def greet(name: str) -> str: pass'), indent=2))"
```

In the output, notice:
- `arg(arg='name', annotation=Name(id='str', ...))` — the **`annotation`** field
- `returns=Name(id='str', ...)` on `FunctionDef` — the **`returns`** field

These are standard Python AST field names. The rule
`accept(lhs=null, rhs=X, key=["returns", "annotation"])` says: when an AST
field named `returns` or `annotation` was absent (null) and now has a value
(X), accept that change. Without `key`, the rule would apply to any
null-to-value change anywhere in the AST, which would be too broad.

**Config** (`configs/03_accept_annotations.py`):

```python
from ast import *
from infer_semdiff import var, ignore, rewrite, accept, null

X = var("X")

accept(lhs=null, rhs=X, key=["returns", "annotation"])
```

**Before** (`examples/03_add_annotations/before.py`):

```python
def greet(name):
    return "Hello, " + name
```

**After** (`examples/03_add_annotations/after.py`):

```python
def greet(name: str) -> str:
    return "Hello, " + name
```

**Run:**

```bash
infer semdiff \
    --semdiff-configuration configs/03_accept_annotations.py \
    --semdiff-previous examples/03_add_annotations/before.py \
    --semdiff-current examples/03_add_annotations/after.py \
    -o /tmp/semdiff-out

jq . /tmp/semdiff-out/semdiff.json
```

**Output:**

```json
{
  "previous": "examples/03_add_annotations/before.py",
  "current": "examples/03_add_annotations/after.py",
  "outcome": "equal",
  "diff": []
}
```

---

## Scenario 4: `accept()` with `condition` — Rejecting `Any`

**Concept:** The `condition=` parameter adds a guard to an accept rule.
Available predicates are `equals(pat1, pat2)` and `contains(pat1, pat2)`,
composable with `not` and `and`.

During a type annotation migration, adding `Any` as a return type is
technically valid but undesirable — it hides real type information. We want to
reject it. But how do we refer to `Any` in a pattern? Use `ast.dump()`:

```bash
python3 -c "import ast; print(ast.dump(ast.parse('x: Any'), indent=2))"
```

```
Module(
  body=[
    AnnAssign(
      target=Name(id='x', ctx=Store()),
      annotation=Name(id='Any', ctx=Load()),
      simple=1)],
  type_ignores=[])
```

In the output, the `Any` identifier appears as `Name(id='Any', ctx=Load())`.
`Name` is the AST node for any identifier, `id` is the name string, and
`ctx=Load()` means the name is being read (as opposed to `Store()` for
assignment targets). So the pattern `Name(ctx=Load(), id="Any")` matches
exactly the `Any` identifier used as a type annotation.

The condition `not equals(Name(ctx=Load(), id="Any"), X)` then says: accept
the new annotation `X`, but only if `X` is not the `Any` identifier.

**Config** (`configs/04_accept_with_condition.py`):

```python
from ast import *
from infer_semdiff import var, ignore, rewrite, accept, null

X = var("X")
T1 = var("T1")
T2 = var("T2")

accept(
    lhs=null,
    rhs=X,
    condition=not equals(Name(ctx=Load(), id="Any"), X),
    key=["returns", "annotation"]
)

accept(
    lhs=T1,
    rhs=T2,
    condition=(not (equals(null, T1))) and (not (contains(Name(ctx=Load(), id="Any"), T2))),
    key=["returns", "annotation"]
)
```

The second rule handles the case where an annotation already existed: it
accepts the new annotation `T2` only if `T1` was not null (it was already
annotated) and `T2` does not contain `Any` anywhere in its subtree.

**Before** (`examples/04_annotation_with_any/before.py`):

```python
from typing import Any

def fetch(url):
    return download(url)

def parse(data):
    return data.split(",")
```

**After** (`examples/04_annotation_with_any/after.py`):

```python
from typing import Any

def fetch(url: str) -> Any:
    return download(url)

def parse(data: str) -> list[str]:
    return data.split(",")
```

Note: the import line is kept identical in both files so this scenario focuses
purely on the annotation changes, otherwise we would need to use the `ignore`
command explained in Scenario 1.

**Run with condition (rejects Any):**

```bash
infer semdiff \
    --semdiff-configuration configs/04_accept_with_condition.py \
    --semdiff-previous examples/04_annotation_with_any/before.py \
    --semdiff-current examples/04_annotation_with_any/after.py \
    -o /tmp/semdiff-out

jq . /tmp/semdiff-out/semdiff.json
```

**Output:**

```json
{
  "previous": "examples/04_annotation_with_any/before.py",
  "current": "examples/04_annotation_with_any/after.py",
  "outcome": "different",
  "diff": [
    "(Line 3) + def fetch(url: str) -> Any:"
  ]
}
```

The `-> Any` return type on `fetch` is rejected by the condition. The diff
output pinpoints exactly where the rejected change is.

**Compare with the config from Scenario 3** (no condition):

```bash
infer semdiff \
    --semdiff-configuration configs/03_accept_annotations.py \
    --semdiff-previous examples/04_annotation_with_any/before.py \
    --semdiff-current examples/04_annotation_with_any/after.py \
    -o /tmp/semdiff-out

jq . /tmp/semdiff-out/semdiff.json
```

**Output:**

```json
{
  "previous": "examples/04_annotation_with_any/before.py",
  "current": "examples/04_annotation_with_any/after.py",
  "outcome": "equal",
  "diff": []
}
```

Without the condition, `Any` is accepted like any other annotation.

**Warning:** The second accept rule (`lhs=T1, rhs=T2`) accepts any annotation
change as long as the new type does not contain `Any`. However, accepting an
arbitrary type change does not guarantee that the resulting file is well-typed.
For example, changing `int` to `str` would be accepted by this rule even though
it could introduce type errors. Semdiff only checks that the change matches the
configured rules — it does not perform type checking. Users should run a type
checker on the modified file to verify it is still correctly typed.

---

## Scenario 5: Full config — Combined changes

**Concept:** All rules combined into a single configuration file that handles
imports, assignments, and annotations.

**Config** (`configs/05_full.py` — combines all rules from scenarios 1-4):

```python
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
```

**Before** (`examples/05_combined/before.py`):

```python
import os

def lookup(table, key):
    value = table.get(key)
    return value

def format_entry(name):
    if name is None:
        return "<unknown>"
    return name.upper()
```

**After** (`examples/05_combined/after.py`):

```python
from pathlib import Path

def lookup(table: str, key: str) -> int:
    value: int = table.get(key)
    return value

def format_entry(name: str) -> str:
    if name is None:
        return "<unknown>"
    return name.upper()
```

Changes: imports replaced, annotations added, `value = ...` -> `value: int = ...`.
All are cosmetic.

**Run:**

```bash
infer semdiff \
    --semdiff-configuration configs/05_full.py \
    --semdiff-previous examples/05_combined/before.py \
    --semdiff-current examples/05_combined/after.py \
    -o /tmp/semdiff-out

jq . /tmp/semdiff-out/semdiff.json
```

**Output:**

```json
{
  "previous": "examples/05_combined/before.py",
  "current": "examples/05_combined/after.py",
  "outcome": "equal",
  "diff": []
}
```

---

## Scenario 6: Catching real semantic changes

**Concept:** The configuration accepts cosmetic changes but correctly
detects real semantic modifications.

**Config:** Same `configs/05_full.py`

**Before** (`examples/06_real_change/before.py`):

```python
import os

def increment(x):
    return x + 1
```

**After** (`examples/06_real_change/after.py`):

```python
from pathlib import Path

def increment(x: int) -> int:
    return x + 2
```

The import change and added annotations are cosmetic (handled by the config).
But `return x + 1` -> `return x + 2` is a real semantic change.

**Run:**

```bash
infer semdiff \
    --semdiff-configuration configs/05_full.py \
    --semdiff-previous examples/06_real_change/before.py \
    --semdiff-current examples/06_real_change/after.py \
    -o /tmp/semdiff-out

jq . /tmp/semdiff-out/semdiff.json
```

**Output:**

```json
{
  "previous": "examples/06_real_change/before.py",
  "current": "examples/06_real_change/after.py",
  "outcome": "different",
  "diff": [
    "(Line 4) -     return x + 1, +     return x + 2"
  ]
}
```

The config correctly accepts all the cosmetic changes (import, annotations)
but catches the semantic change in the return value. The diff output shows
exactly what changed.

---

## Configuration reference

### Metavariables

```python
X = var("X")   # Declares a metavariable that matches any AST subtree
```

### Rule types

| Rule | Syntax | Semantics |
|------|--------|-----------|
| `ignore` | `ignore(pattern)` | Filter out matching nodes from both sides |
| `rewrite` | `rewrite(lhs=..., rhs=...)` | Bidirectional: both forms are equivalent |
| `accept` | `accept(lhs=..., rhs=..., key=..., condition=...)` | Unidirectional: accept `lhs`->`rhs` change |

### Patterns

| Syntax | Matches |
|--------|---------|
| `Name(id="str", ctx=Load())` | A specific AST node with given fields |
| `X` (metavariable) | Any AST subtree (binds for reuse) |
| `null` | Absent/None field value |
| `"str"` | A string literal |
| `1` | An integer literal |
| `[X, Y]` | A list with specific elements |

### The `key` parameter

Restricts an `accept` rule to specific AST field positions. Values are Python
AST field names (discoverable via `ast.dump()`):

| Key | Where it appears |
|-----|-----------------|
| `"annotation"` | Type annotation on function arguments (`arg.annotation`) |
| `"returns"` | Return type on function definitions (`FunctionDef.returns`) |

### Conditions

| Syntax | Meaning |
|--------|---------|
| `equals(pat1, pat2)` | Both patterns evaluate to the same AST node |
| `contains(pat1, pat2)` | `pat2` recursively contains a subtree matching `pat1` |
| `not expr` | Boolean negation |
| `expr1 and expr2` | Boolean conjunction |
