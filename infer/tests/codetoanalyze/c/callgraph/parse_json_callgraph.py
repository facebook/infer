# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#!/usr/bin/env python3
"""Parse infer's specialized_call_graph.json and list call-graph edges."""

import json
import sys


def main():
    path = sys.argv[1] if len(sys.argv) > 1 else "infer-out/specialized_call_graph.json"
    with open(path) as f:
        data = json.load(f)

    try:
        edges, contexts = data["edges"], data["contexts"]
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
    except (KeyError, IndexError) as e:
        print(f"Error: malformed call graph in {path}: {e}", file=sys.stderr)
        sys.exit(1)

    lines.sort(key=lambda e: (e[0], e[1], e[2], e[3]))
    for _, _, _, edge in lines:
        print(edge)


if __name__ == "__main__":
    main()
