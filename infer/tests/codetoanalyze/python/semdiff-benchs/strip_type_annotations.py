#!/usr/bin/env python3
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""Strip function type annotations from a Python source file.

Removes:
- Function parameter annotations (def foo(x: int) -> def foo(x))
- Function return annotations (def foo() -> int -> def foo())
- Variable annotations with value (x: int = 1 -> x = 1)

Keeps:
- Bare variable/field annotations (x: int) to preserve class structure

Usage:
    python3 strip_type_annotations.py input.py > output.py
    python3 strip_type_annotations.py input.py -o output.py
"""

import argparse
import ast
import sys


class AnnotationStripper(ast.NodeTransformer):
    def visit_FunctionDef(self, node):
        node.returns = None
        self._strip_args(node.args)
        self.generic_visit(node)
        return node

    def visit_AsyncFunctionDef(self, node):
        node.returns = None
        self._strip_args(node.args)
        self.generic_visit(node)
        return node

    def visit_AnnAssign(self, node):
        self.generic_visit(node)
        if node.value is None:
            # x: int  (no value) -> keep as-is to preserve class structure
            return node
        # x: int = 1 -> x = 1
        return ast.Assign(
            targets=[node.target],
            value=node.value,
            lineno=node.lineno,
            col_offset=node.col_offset,
            end_lineno=node.end_lineno,
            end_col_offset=node.end_col_offset,
        )

    def _strip_args(self, arguments):
        for arg in arguments.args:
            arg.annotation = None
        for arg in arguments.posonlyargs:
            arg.annotation = None
        for arg in arguments.kwonlyargs:
            arg.annotation = None
        if arguments.vararg:
            arguments.vararg.annotation = None
        if arguments.kwarg:
            arguments.kwarg.annotation = None


def strip_annotations(source):
    tree = ast.parse(source)
    stripper = AnnotationStripper()
    tree = stripper.visit(tree)
    ast.fix_missing_locations(tree)
    return ast.unparse(tree)


def main():
    parser = argparse.ArgumentParser(
        description="Strip all type annotations from a Python file."
    )
    parser.add_argument("input", help="Input Python file")
    parser.add_argument(
        "-o", "--output", help="Output file (default: stdout)", default=None
    )
    args = parser.parse_args()

    with open(args.input) as f:
        source = f.read()

    result = strip_annotations(source)

    if args.output:
        with open(args.output, "w") as f:
            f.write(result)
            f.write("\n")
    else:
        print(result)


if __name__ == "__main__":
    main()
