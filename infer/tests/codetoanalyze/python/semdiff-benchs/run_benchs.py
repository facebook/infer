#!/usr/bin/env python3
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""Benchmark semdiff engines (DirectRewrite vs Eqsat) on Python files.

Strips type annotations from source files, then compares original vs stripped
using both engines. Outputs a per-file table and a summary by size bucket.

Usage:
    python3 run_benchs.py <source_dir> [--max-lines N] [--timeout S] [--config FILE]

Example:
    python3 run_benchs.py /tmp/pydantic/pydantic --max-lines 300 --timeout 60
"""

import argparse
import json
import os
import re
import subprocess
import sys
import tempfile
import time
from strip_type_annotations import strip_annotations as strip_source


def find_python_files(source_dir):
    """Find all .py files excluding tests and __pycache__."""
    results = []
    for root, dirs, files in os.walk(source_dir):
        dirs[:] = [d for d in dirs if d != "__pycache__" and "test" not in d.lower()]
        for f in files:
            if f.endswith(".py") and "test" not in f.lower():
                results.append(os.path.join(root, f))
    return results


def strip_all_annotations(source_dir, files, stripped_dir):
    """Strip type annotations from all files into stripped_dir."""
    for f in files:
        rel = os.path.relpath(f, source_dir)
        unique = rel.replace(os.sep, "__")
        out = os.path.join(stripped_dir, unique)
        with open(f) as fh:
            source = fh.read()
        result = strip_source(source)
        with open(out, "w") as fh:
            fh.write(result)
            fh.write("\n")


def parse_egraph_stats(stdout_text):
    """Parse debug output to extract total atoms visited and merges."""
    total_visits = 0
    total_merges = 0
    for m in re.finditer(r'(\d+) atoms visited, (\d+) merges', stdout_text):
        total_visits += int(m.group(1))
        total_merges += int(m.group(2))
    rounds = len(re.findall(r'full_rewrite - round', stdout_text))
    return {"visits": total_visits, "merges": total_merges, "rounds": rounds}


def run_semdiff(infer, engine, config, previous, current, timeout):
    """Run infer semdiff and return (time_ms, outcome, stats)."""
    cmd = [str(infer), "semdiff"]
    if engine == "eqsat":
        cmd.append("--semdiff-experimental-eqsat-engine")
        cmd.append("--debug")
    else:
        cmd.extend(["--semdiff-configuration", str(config)])
    cmd.extend(["--semdiff-previous", previous, "--semdiff-current", current])

    start = time.monotonic()
    try:
        proc = subprocess.run(
            cmd, timeout=timeout, capture_output=True, check=False, text=True,
        )
        elapsed_ms = int((time.monotonic() - start) * 1000)
        result_file = "infer-out/semdiff.json"
        try:
            with open(result_file) as f:
                outcome = json.load(f)["outcome"]
        except (FileNotFoundError, json.JSONDecodeError, KeyError):
            outcome = "ERROR"
        stats = parse_egraph_stats(proc.stdout) if engine == "eqsat" else None
        return elapsed_ms, outcome, stats
    except subprocess.TimeoutExpired:
        elapsed_ms = int((time.monotonic() - start) * 1000)
        return elapsed_ms, "TIMEOUT", None


def fmt_result(ms, outcome):
    short = {"different": "diff", "TIMEOUT": "T/O", "CRASH": "CRASH", "ERROR": "ERR", "N/A": "N/A"}
    label = short.get(outcome, outcome)
    if outcome in ("TIMEOUT", "CRASH", "ERROR", "N/A"):
        return label
    return f"{label} {ms}ms"


def print_tables(results, max_lines):
    """Print per-file table and summary by size bucket."""
    all_files = sorted(results.keys(), key=lambda n: results[n]["lines"])

    name_w = max((len(n) for n in all_files), default=20)
    header = (f"{'File':<{name_w}}  {'Lines':>5}  {'Direct':>14}  {'Eqsat':>14}  {'Match':>5}"
              f"  {'Visits':>8} {'Merges':>7} {'Rnds':>4}  {'V/M':>6}")
    sep = "-" * len(header)

    print()
    print(header)
    print(sep)

    buckets = []
    step = 50
    lo = 0
    while lo <= max_lines:
        hi = min(lo + step - 1, max_lines)
        if lo == 0:
            hi = 10
        buckets.append((lo, hi))
        lo = hi + 1
        if step < 100:
            step = 50 if lo <= 50 else 100

    # Simpler fixed buckets
    buckets = [(0, 10), (11, 50), (51, 100), (101, 150), (151, 200), (201, 300),
               (301, 500), (501, 1000), (1001, 10000)]
    buckets = [(lo, hi) for lo, hi in buckets if lo <= max_lines]

    stats = {b: dict(count=0, d_eq=0, d_diff=0, d_to=0, e_eq=0, e_diff=0, e_to=0,
                      d_ms_sum=0, e_ms_sum=0, d_ms_n=0, e_ms_n=0)
             for b in buckets}

    for name in all_files:
        r = results[name]
        lines = r["lines"]
        d_ms, d_out = r.get("direct", (0, "N/A"))
        e_ms, e_out = r.get("eqsat", (0, "N/A"))
        match = "yes" if d_out == e_out else "NO"
        es = r.get("egraph_stats", {})
        visits = es.get("visits", 0)
        merges = es.get("merges", 0)
        rounds = es.get("rounds", 0)
        vm_ratio = f"{visits / merges:.0f}" if merges > 0 else "-"
        print(f"{name:<{name_w}}  {lines:>5}  {fmt_result(d_ms, d_out):>14}  "
              f"{fmt_result(e_ms, e_out):>14}  {match:>5}"
              f"  {visits:>8} {merges:>7} {rounds:>4}  {vm_ratio:>6}")

        for lo, hi in buckets:
            if lo <= lines <= hi:
                s = stats[(lo, hi)]
                s["count"] += 1
                s["d_eq"] += d_out == "equal"
                s["d_diff"] += d_out == "different"
                s["d_to"] += d_out == "TIMEOUT"
                s["e_eq"] += e_out == "equal"
                s["e_diff"] += e_out == "different"
                s["e_to"] += e_out == "TIMEOUT"
                if d_out != "TIMEOUT":
                    s["d_ms_sum"] += d_ms
                    s["d_ms_n"] += 1
                if e_out != "TIMEOUT":
                    s["e_ms_sum"] += e_ms
                    s["e_ms_n"] += 1
                break

    # Summary
    print()
    print("=" * 90)
    print("SUMMARY BY SIZE BUCKET")
    print("=" * 90)

    col = (f"{'Lines':>10}  {'Count':>5}  |  {'eq':>3} {'diff':>4} {'t/o':>3} {'avg':>6}  "
           f"|  {'eq':>3} {'diff':>4} {'t/o':>3} {'avg':>6}  | {'E/D':>5}")
    hdr = f"{'':>10}  {'':>5}  |{'Direct':^23}|{'Eqsat':^23}|"
    print()
    print(hdr)
    print(col)
    print("-" * len(col))

    tot = dict(count=0, d_eq=0, d_diff=0, d_to=0, e_eq=0, e_diff=0, e_to=0,
               d_ms_sum=0, e_ms_sum=0, d_ms_n=0, e_ms_n=0)

    def fmt_avg(ms_sum, n):
        if n == 0:
            return "  -"
        return f"{ms_sum // n}ms"

    def fmt_ratio(d_sum, d_n, e_sum, e_n):
        if d_n == 0 or e_n == 0:
            return "  -"
        d_avg = d_sum / d_n
        e_avg = e_sum / e_n
        if d_avg == 0:
            return "  -"
        return f"{e_avg / d_avg:.1f}x"

    for lo, hi in buckets:
        s = stats[(lo, hi)]
        if s["count"] == 0:
            continue
        label = f"{lo}-{hi}"
        print(f"{label:>10}  {s['count']:>5}  |  {s['d_eq']:>3} {s['d_diff']:>4} "
              f"{s['d_to']:>3} {fmt_avg(s['d_ms_sum'], s['d_ms_n']):>6}  "
              f"|  {s['e_eq']:>3} {s['e_diff']:>4} {s['e_to']:>3} {fmt_avg(s['e_ms_sum'], s['e_ms_n']):>6}  "
              f"| {fmt_ratio(s['d_ms_sum'], s['d_ms_n'], s['e_ms_sum'], s['e_ms_n']):>5}")
        for k in tot:
            tot[k] += s[k]

    print("-" * len(col))
    print(f"{'TOTAL':>10}  {tot['count']:>5}  |  {tot['d_eq']:>3} {tot['d_diff']:>4} "
          f"{tot['d_to']:>3} {fmt_avg(tot['d_ms_sum'], tot['d_ms_n']):>6}  "
          f"|  {tot['e_eq']:>3} {tot['e_diff']:>4} {tot['e_to']:>3} {fmt_avg(tot['e_ms_sum'], tot['e_ms_n']):>6}  "
          f"| {fmt_ratio(tot['d_ms_sum'], tot['d_ms_n'], tot['e_ms_sum'], tot['e_ms_n']):>5}")
    print()


def main():
    parser = argparse.ArgumentParser(
        description="Benchmark semdiff engines (DirectRewrite vs Eqsat)."
    )
    parser.add_argument("source_dir", help="Directory of Python source files")
    parser.add_argument("--max-lines", type=int, required=True,
                        help="Only benchmark files with at most N lines")
    parser.add_argument("--timeout", type=int, required=True,
                        help="Timeout in seconds per file")
    parser.add_argument("--config", type=str, required=True,
                        help="DirectRewrite config file")
    parser.add_argument("--infer", type=str, default="infer",
                        help="Path to infer binary (default: from PATH)")
    args = parser.parse_args()

    source_dir = os.path.abspath(args.source_dir)
    infer = args.infer
    config = os.path.abspath(args.config)

    # Find and filter files
    all_files = find_python_files(source_dir)
    files_with_lines = []
    for f in all_files:
        with open(f) as fh:
            lines = sum(1 for _ in fh)
        if lines <= args.max_lines:
            files_with_lines.append((lines, f))
    files_with_lines.sort()

    print(f"Found {len(files_with_lines)} files (<= {args.max_lines} lines)", file=sys.stderr)

    # Strip annotations into temp dir
    with tempfile.TemporaryDirectory(prefix="semdiff_bench_") as workdir:
        stripped_dir = os.path.join(workdir, "stripped")
        os.makedirs(stripped_dir)

        print("Stripping type annotations...", file=sys.stderr)
        strip_all_annotations(source_dir, [f for _, f in files_with_lines], stripped_dir)

        # Run both engines
        results = {}
        for engine in ("direct", "eqsat"):
            print(f"\n=== Running {engine} ===", file=sys.stderr)
            for lines, filepath in files_with_lines:
                rel = os.path.relpath(filepath, source_dir)
                unique = rel.replace(os.sep, "__")
                stripped = os.path.join(stripped_dir, unique)
                if not os.path.exists(stripped):
                    continue

                ms, outcome, stats = run_semdiff(infer, engine, config, stripped, filepath, args.timeout)
                print(f"  [{engine}] {lines} lines  {ms}ms  {outcome}  {rel}", file=sys.stderr)

                if rel not in results:
                    results[rel] = {"lines": lines}
                results[rel][engine] = (ms, outcome)
                if stats:
                    results[rel]["egraph_stats"] = stats

    print_tables(results, args.max_lines)


if __name__ == "__main__":
    main()
