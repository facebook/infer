# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#!/usr/bin/env python3
"""Parse infer's specialized_call_graph.json and list call-graph edges."""

import json
import sys


def format_heap_path(hp):
    """Format a heap path from ATD-generated JSON.

    ATD variants are encoded as ["Tag", payload]:
      ["Pvar", "x"]
      ["FieldAccess", {"field": {"class_name": "...", "field_name": "f"}, "base": <hp>}]
      ["Dereference", <hp>]

    We produce a compact C-like notation:
      Pvar(x)                           -> x
      Dereference(Pvar(x))              -> x
      FieldAccess(f, Dereference(Pvar(x))) -> x->f
      Dereference(FieldAccess(f, Dereference(Pvar(x)))) -> *(x->f)
    """
    tag = hp[0]
    payload = hp[1]
    if tag == "Pvar":
        return payload
    if tag == "Dereference":
        inner_tag = payload[0]
        if inner_tag == "Pvar":
            # Dereference(Pvar(x)) -> just "x"
            return payload[1]
        if inner_tag == "FieldAccess":
            # Dereference(FieldAccess(f, base))
            field_name = payload[1]["field"]["field_name"]
            base = payload[1]["base"]
            base_str = format_heap_path(base)
            return f"*({base_str}->{field_name})"
        inner_str = format_heap_path(payload)
        return f"*({inner_str})"
    if tag == "FieldAccess":
        field_name = payload["field"]["field_name"]
        base_str = format_heap_path(payload["base"])
        return f"{base_str}->{field_name}"
    return str(hp)


def format_dynamic_type(dt):
    """Format a dynamic type entry like Dereference(f):add."""
    path_str = format_heap_path(dt["path"])
    return f"{path_str}:{dt['typ']}"


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


def extract_edges(contexts, caller, callees, edges):
    """Extract call-graph edges from a non_disj list."""
    for callee_record in callees:
        callee = callee_record["callee_name"]
        loc = callee_record["call_location"]
        file = loc["file"]
        line = loc["line"]
        col = loc["col"]
        spec = contexts[int(callee_record.get("callee_context", 0))]
        spec_str = format_specialization(spec)
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
        caller_spec = contexts[int(caller["caller_context"])]
        effective_caller = caller["caller_name"] + format_specialization(caller_spec)
        extract_edges(contexts, effective_caller, node.get("callees", []), edges)

    edges.sort(key=lambda e: (e[0], e[1], e[2]))
    for _, _, _, edge in edges:
        print(edge)


if __name__ == "__main__":
    main()
