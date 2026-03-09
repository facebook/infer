# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#!/usr/bin/env python3
"""Parse infer's specialized_call_graph.json and list call-graph edges."""

import json
import sys


def get_proc_name(proc):
    """Extract a human-readable name from a proc_name structure."""
    kind = proc[0]
    if kind == "C":
        return "::".join(proc[1]["c_name"])
    raise ValueError(f"Unexpected proc_name kind: {kind!r}, expected 'C'")


def format_heap_path(path_elem):
    """Format a heap path element like Dereference(p)."""
    access = path_elem[0]
    var = path_elem[1][1]["plain"]
    return f"{access}({var})"


def format_dynamic_type(dt):
    """Format a dynamic type entry like Dereference(f):add."""
    heap_path, typ = dt
    path_str = format_heap_path(heap_path)
    kind = typ[0]
    if kind == "CFunction":
        type_str = "::".join(typ[1]["c_name"])
    else:
        type_str = str(typ[1])
    return f"{path_str}:{type_str}"


def format_specialization(spec):
    """Format specialization as [aliases:...] or [types:...]."""
    aliases = spec.get("aliases")
    dynamic_types = spec.get("dynamic_types", [])
    if aliases and isinstance(aliases, list):
        groups = []
        for alias_group in aliases:
            groups.append("=".join(format_heap_path(p) for p in alias_group))
        return " [aliases:" + ",".join(groups) + "]"
    if dynamic_types:
        return " [types:" + ",".join(format_dynamic_type(dt) for dt in dynamic_types) + "]"
    return ""


def extract_edges(caller, non_disj, edges):
    """Extract call-graph edges from a non_disj list."""
    for callee_record in non_disj:
        if isinstance(callee_record, str):
            continue
        callee = get_proc_name(callee_record["proc_name"])
        loc = callee_record["loc"]
        file = loc["file"][1] if isinstance(loc["file"], list) else loc["file"]
        line = loc["line"]
        col = loc["col"]
        spec = callee_record.get("specialization", {})
        spec_str = format_specialization(spec)
        edges.append((file, line, col, f"{file}:{line}:{col} {caller} ===> {callee}{spec_str}"))


def main():
    path = sys.argv[1] if len(sys.argv) > 1 else "infer-out/specialized_call_graph.json"
    with open(path) as f:
        data = json.load(f)

    edges = []
    for entry in data:
        caller_proc, summaries = entry
        caller = get_proc_name(caller_proc)

        for checker_name, summary in summaries:
            if checker_name != "pulse":
                continue
            extract_edges(caller, summary["main"].get("non_disj", []), edges)
            for specialized_entry in summary.get("specialized", []):
                specialization, specialized_summary = specialized_entry
                specialized_caller = caller + format_specialization(specialization)
                extract_edges(specialized_caller, specialized_summary.get("non_disj", []), edges)

    edges.sort(key=lambda e: (e[0], e[1], e[2]))
    for _, _, _, edge in edges:
        print(edge)


if __name__ == "__main__":
    main()
