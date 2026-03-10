# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#!/usr/bin/env python3
"""Parse infer's specialized_call_graph.json and list call-graph edges."""

import json
import sys


def extract_edges(contexts, caller, callees, edges):
    """Extract call-graph edges from a callees list."""
    for callee_record in callees:
        callee = callee_record["callee_name"]
        loc = callee_record["call_location"]
        file = loc["file"]
        line = loc["line"]
        col = loc["col"]
        spec_str = contexts[int(callee_record.get("callee_context", 0))]
        edges.append((file, line, col, f"{file}:{line}:{col} {caller} ===> {callee}{spec_str}"))


def main():
    path = sys.argv[1] if len(sys.argv) > 1 else "infer-out/specialized_call_graph.json"
    with open(path) as f:
        data = json.load(f)

    nodes = data["nodes"]
    contexts = data["contexts"]
    edges = []
    for node in nodes:
        caller = node["caller"]
        caller_spec_str = contexts[int(caller["caller_context"])]
        effective_caller = caller["caller_name"] + caller_spec_str
        extract_edges(contexts, effective_caller, node.get("callees", []), edges)

    edges.sort(key=lambda e: (e[0], e[1], e[2]))
    for _, _, _, edge in edges:
        print(edge)


if __name__ == "__main__":
    main()
