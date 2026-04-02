#!/usr/bin/env python3
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""Mutate a Python source file by replacing the deepest `return x` with `return None`.

Only targets `return x` where x is not None. Among all such returns,
picks the one at the greatest AST depth (most nested).

Usage:
    python3 mutate_return.py input.py
    python3 mutate_return.py input.py -o output.py
"""

import argparse
import ast
import sys


def _find_deepest_return(node, depth=0):
    """Find the deepest return statement with a non-None value."""
    best = None
    best_depth = -1
    if isinstance(node, ast.Return) and node.value is not None:
        if not (isinstance(node.value, ast.Constant) and node.value.value is None):
            best = node
            best_depth = depth
    for child in ast.iter_child_nodes(node):
        candidate, d = _find_deepest_return(child, depth + 1)
        if candidate is not None and d > best_depth:
            best = candidate
            best_depth = d
    return best, best_depth


class _ReturnMutator(ast.NodeTransformer):
    def __init__(self, target_node):
        self.target = target_node

    def visit_Return(self, node):
        if node is self.target:
            return ast.Return(
                value=ast.Constant(value=None),
                lineno=node.lineno,
                col_offset=node.col_offset,
                end_lineno=node.end_lineno,
                end_col_offset=node.end_col_offset,
            )
        return node


def mutate_return(source):
    """Replace the deepest `return x` (x != None) with `return None`.

    Returns the mutated source string, or None if no suitable return was found.
    """
    tree = ast.parse(source)
    target, _ = _find_deepest_return(tree)
    if target is None:
        return None
    mutator = _ReturnMutator(target)
    tree = mutator.visit(tree)
    ast.fix_missing_locations(tree)
    return ast.unparse(tree)


def main():
    parser = argparse.ArgumentParser(
        description="Mutate deepest return statement to return None."
    )
    parser.add_argument("input", help="Input Python file")
    parser.add_argument(
        "-o", "--output", help="Output file (default: stdout)", default=None
    )
    args = parser.parse_args()

    with open(args.input) as f:
        source = f.read()

    result = mutate_return(source)
    if result is None:
        print("No suitable return statement found", file=sys.stderr)
        sys.exit(1)

    if args.output:
        with open(args.output, "w") as f:
            f.write(result)
            f.write("\n")
    else:
        print(result)


if __name__ == "__main__":
    main()
