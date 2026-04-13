#!/usr/bin/env python3
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""Generate semdiff batch JSON files from Python source directories.

For each Python file, strips type annotations to produce a "previous" version,
and pairs it with the original as "current" (stripped batch). Optionally also
generates mutated pairs (return mutation) for negative testing.

The output JSON matches the ATD schema in infer/src/atd/semdiff_batch.atd.

Usage:
    python3 gen_batch_json.py <source_dir>... --max-lines N -o batch.json
    python3 gen_batch_json.py <source_dir>... --max-lines N -o batch.json --output-mutated batch_mut.json
"""

import argparse
import ast
import json
import os
import sys
import time
from strip_type_annotations import strip_annotations as strip_source
from mutate_return import mutate_return


def node_to_dict(node):
    """Convert an ast.AST node to the dict format expected by PythonSourceAst.Node.of_yojson."""
    if isinstance(node, ast.AST):
        result = {"_type": node.__class__.__name__}
        for attr in ("lineno", "end_lineno"):
            if hasattr(node, attr):
                result[attr] = getattr(node, attr)
        for field, value in ast.iter_fields(node):
            result[field] = node_to_dict(value)
        return result
    elif isinstance(node, list):
        return [node_to_dict(x) for x in node]
    elif isinstance(node, bytes):
        return "<unserializable bytes>"
    elif isinstance(node, complex):
        return {"_type": "complex", "real": node.real, "imag": node.imag}
    elif node is Ellipsis:
        return "..."
    elif isinstance(node, str):
        return node
    else:
        return node


def source_to_entry(filename, source):
    """Parse source and return a semdiff_file entry."""
    tree = ast.parse(source)
    return {"filename": filename, "source": source, "ast": node_to_dict(tree)}


def find_python_files(source_dir):
    """Find all .py files excluding tests and __pycache__."""
    results = []
    for root, dirs, files in os.walk(source_dir):
        dirs[:] = [d for d in dirs if d != "__pycache__" and "test" not in d.lower()]
        for f in files:
            if f.endswith(".py") and "test" not in f.lower():
                results.append(os.path.join(root, f))
    return results


def write_batch(path, pairs):
    """Write a batch JSON file and return its size in KB."""
    batch = {"language": "python", "pairs": pairs}
    with open(path, "w") as f:
        json.dump(batch, f)
    return os.path.getsize(path) // 1024


def main():
    parser = argparse.ArgumentParser(
        description="Generate semdiff batch JSON files from Python source directories."
    )
    parser.add_argument("source_dir", nargs="+", help="Directories of Python source files")
    parser.add_argument("--max-lines", type=int, required=True,
                        help="Only include files with at most N lines")
    parser.add_argument("--min-lines", type=int, default=0,
                        help="Only include files with at least N lines")
    parser.add_argument("-o", "--output", required=True,
                        help="Output JSON file (stripped vs original)")
    parser.add_argument("--output-mutated", type=str, default=None,
                        help="Output JSON file (stripped vs mutated)")
    args = parser.parse_args()

    # Collect files from all source dirs
    files_with_lines = []
    for source_dir in args.source_dir:
        source_dir = os.path.abspath(source_dir)
        for f in find_python_files(source_dir):
            with open(f) as fh:
                lines = sum(1 for _ in fh)
            if args.min_lines <= lines <= args.max_lines:
                files_with_lines.append((lines, f, source_dir))
    files_with_lines.sort()

    stripped_pairs = []
    mutated_pairs = []
    file_lines = []
    skipped_mut = 0
    skipped_err = 0
    start = time.monotonic()

    for lines, filepath, source_dir in files_with_lines:
        rel = os.path.relpath(filepath, source_dir)
        with open(filepath) as fh:
            source = fh.read()

        try:
            stripped = strip_source(source)
            previous = source_to_entry(rel, stripped)
            current = source_to_entry(rel, source)
        except SyntaxError as e:
            print(f"  skipping {rel}: {e}", file=sys.stderr)
            skipped_err += 1
            continue

        stripped_pairs.append({"previous": previous, "current": current})
        file_lines.append((rel, lines))

        if args.output_mutated is not None:
            mutated = mutate_return(source)
            if mutated is not None:
                try:
                    mut_current = source_to_entry(rel, mutated)
                    mutated_pairs.append({"previous": previous, "current": mut_current})
                except SyntaxError:
                    skipped_mut += 1
            else:
                skipped_mut += 1

    elapsed_ms = int((time.monotonic() - start) * 1000)

    # Write batch files
    stripped_kb = write_batch(args.output, stripped_pairs)
    mutated_kb = None
    if args.output_mutated is not None:
        mutated_kb = write_batch(args.output_mutated, mutated_pairs)

    # Print file list
    total_lines = sum(l for _, l in file_lines)
    print(f"\n  {len(file_lines)} files, {total_lines} total lines "
          f"({skipped_err} skipped)", file=sys.stderr)
    for rel, lines in file_lines:
        print(f"    {lines:>5} lines  {rel}", file=sys.stderr)

    # Print summary
    print(file=sys.stderr)
    print("=== Capture summary ===", file=sys.stderr)
    print(f"  stripped: {len(stripped_pairs)} pairs, {stripped_kb}KB"
          f"  -> {args.output}", file=sys.stderr)
    if args.output_mutated is not None:
        print(f"  mutated:  {len(mutated_pairs)} pairs, {mutated_kb}KB"
              f"  ({skipped_mut} skipped)"
              f"  -> {args.output_mutated}", file=sys.stderr)
        total_pairs = len(stripped_pairs) + len(mutated_pairs)
    else:
        total_pairs = len(stripped_pairs)
    print(f"  total:    {total_pairs} pairs, {elapsed_ms}ms", file=sys.stderr)


if __name__ == "__main__":
    main()
