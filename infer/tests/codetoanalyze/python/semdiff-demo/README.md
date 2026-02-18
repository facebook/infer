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
infer semdiff --quiet --no-progress-bar \
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
infer semdiff --quiet --no-progress-bar \
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
