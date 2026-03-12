# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#!/usr/bin/env python3
"""Parse infer's specialized_call_graph.json and list call-graph edges."""

import json
import sys


def node_str(edges, contexts, index):
    """Return the display string for the node at the given edge index."""
    caller = edges[index]["caller"]
    return caller["name"] + contexts[caller["context"]]


def node_str_compact(edges, index):
    """Return a compact display string: name#context (omitting #0)."""
    caller = edges[index]["caller"]
    ctx = caller["context"]
    return caller["name"] if ctx == 0 else f'{caller["name"]}#{ctx}'


def print_edges(edges, contexts):
    """Print all call-graph edges sorted by location."""
    lines = []
    for edge in edges:
        caller = edge["caller"]
        caller_spec = contexts[caller["context"]]
        caller_str = caller["name"] + caller_spec
        for callee_record in edge["callees"]:
            callee = edges[callee_record["callee"]]["caller"]
            callee_spec = contexts[callee["context"]]
            callee_str = callee["name"] + callee_spec
            loc = callee_record["call_location"]
            file, line, col = loc["file"], loc["line"], loc["col"]
            lines.append((file, line, col, f"{file}:{line}:{col} {caller_str} ===> {callee_str}"))

    lines.sort(key=lambda e: (e[0], e[1], e[2], e[3]))
    for _, _, _, line in lines:
        print(line)


def print_paths(edges, contexts, root_names):
    """Print the call tree rooted at each given root name (context=0)."""
    roots = [
        i for i, edge in enumerate(edges)
        if edge["caller"]["context"] == 0 and edge["caller"]["name"] in root_names
    ]

    def walk(index, prefix, connector, on_path):
        name = node_str_compact(edges, index)
        if index in on_path:
            print(prefix + connector + name + " (recursive)")
            return
        print(prefix + connector + name)
        on_path = on_path | {index}
        callees = sorted(
            [r["callee"] for r in edges[index]["callees"]],
            key=lambda c: node_str_compact(edges, c),
        )
        child_prefix = prefix + ("    " if connector == "┗━━ " else "┃   " if connector else "")
        for i, callee_index in enumerate(callees):
            is_last = i == len(callees) - 1
            walk(callee_index, child_prefix, "┗━━ " if is_last else "┣━━ ", on_path)

    for root in sorted(roots, key=lambda i: node_str_compact(edges, i)):
        walk(root, "", "", set())


def main():
    path = sys.argv[1] if len(sys.argv) > 1 else "infer-out/specialized_call_graph.json"
    mode = sys.argv[2] if len(sys.argv) > 2 else "edges"

    with open(path) as f:
        data = json.load(f)

    try:
        edges, contexts = data["edges"], data["contexts"]
        if mode == "paths":
            root_names = set(sys.argv[3:])
            print_paths(edges, contexts, root_names)
        else:
            print_edges(edges, contexts)
    except (KeyError, IndexError) as e:
        print(f"Error: malformed call graph in {path}: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
