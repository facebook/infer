#!/usr/bin/env python3
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""Run semdiff batch benchmarks and report results.

Takes pre-generated batch JSON files (from gen_batch_json.py) and runs
infer semdiff --semdiff-from-json with both engines (direct, eqsat).
Reports per-pair outcomes and a summary.

Usage:
    python3 run_batch_bench.py batch.json [batch_mut.json] [--timeout S] [--config FILE]
"""

import argparse
import json
import os
import subprocess
import sys
import time


def run_semdiff_batch(infer, engine, config, batch_json, timeout, debug=False):
    """Run infer semdiff --semdiff-from-json and return (time_ms, results)."""
    result_file = "infer-out/semdiff.json"
    if os.path.exists(result_file):
        os.remove(result_file)

    cmd = [str(infer), "semdiff", "--semdiff-from-json", batch_json]
    if engine == "eqsat":
        cmd.append("--semdiff-experimental-eqsat-engine")
    if debug:
        cmd.append("--debug")
    else:
        cmd.extend(["--semdiff-configuration", str(config)])

    start = time.monotonic()
    try:
        proc = subprocess.run(cmd, timeout=timeout, capture_output=True, check=False, text=True)
        elapsed_ms = int((time.monotonic() - start) * 1000)

        if proc.returncode != 0:
            print(f"  ERROR: infer returned {proc.returncode}", file=sys.stderr)
            if proc.stderr:
                print(f"  stderr: {proc.stderr[:500]}", file=sys.stderr)
            return elapsed_ms, None

        try:
            with open(result_file) as f:
                results = json.load(f)
            return elapsed_ms, results
        except (FileNotFoundError, json.JSONDecodeError) as e:
            print(f"  ERROR reading results: {e}", file=sys.stderr)
            return elapsed_ms, None

    except subprocess.TimeoutExpired:
        elapsed_ms = int((time.monotonic() - start) * 1000)
        return elapsed_ms, None


def print_results(label, engine, elapsed_ms, results):
    """Print per-pair outcomes and summary for one run."""
    if results is None:
        print(f"\n--- {label} [{engine}] --- FAILED ({elapsed_ms}ms)")
        return

    n_equal = sum(1 for r in results if r["outcome"] == "equal")
    n_diff = sum(1 for r in results if r["outcome"] == "different")
    n_fuel = sum(1 for r in results if r["outcome"] == "fuel_exhausted")
    n_total = len(results)

    fuel_str = f", {n_fuel} fuel_exhausted" if n_fuel > 0 else ""
    print(f"\n--- {label} [{engine}] --- {elapsed_ms}ms total, "
          f"{n_equal} equal, {n_diff} different{fuel_str}, {n_total} pairs")

    for r in results:
        print(f"  {r['outcome']:>10}  {r['previous']}")


def main():
    parser = argparse.ArgumentParser(
        description="Run semdiff batch benchmarks with both engines."
    )
    parser.add_argument("batch_json", help="Batch JSON file (stripped vs original)")
    parser.add_argument("--batch-mut", type=str, default=None,
                        help="Mutated batch JSON file (stripped vs mutated)")
    parser.add_argument("--timeout", type=int, default=300,
                        help="Timeout in seconds per batch invocation (default: 300)")
    parser.add_argument("--config", type=str, required=True,
                        help="DirectRewrite config file")
    parser.add_argument("--infer", type=str, default="infer",
                        help="Path to infer binary (default: from PATH)")
    parser.add_argument("--debug", action="store_true", default=False,
                        help="Pass --debug to infer semdiff")
    args = parser.parse_args()

    config = os.path.abspath(args.config)
    experiments = [("stripped vs original", args.batch_json)]
    if args.batch_mut:
        experiments.append(("stripped vs mutated", args.batch_mut))

    all_runs = []
    for label, batch in experiments:
        for engine in ("direct", "eqsat"):
            print(f"\nRunning {engine} on {label}...", file=sys.stderr)
            ms, results = run_semdiff_batch(
                infer=args.infer, engine=engine, config=config,
                batch_json=batch, timeout=args.timeout, debug=args.debug,
            )
            print_results(label, engine, ms, results)
            all_runs.append((label, engine, ms, results))

    print_summary(all_runs)


def print_summary(all_runs):
    """Print a synthetic summary table."""
    print()
    print("=" * 72)
    print("SUMMARY")
    print("=" * 72)

    hdr = f"{'Experiment':<28} {'Engine':<8} {'Pairs':>5} {'Equal':>5} {'Diff':>5} {'Fuel':>4} {'Time':>8} {'Avg':>8} {'E/D':>6}"
    print(hdr)
    print("-" * len(hdr))

    # Group runs by label to compute eqsat/direct speed factor
    by_label = {}
    for label, engine, ms, results in all_runs:
        by_label.setdefault(label, {})[engine] = (ms, results)

    for label, engine, ms, results in all_runs:
        if results is None:
            print(f"{label:<28} {engine:<8} {'FAILED':>5}")
            continue
        n_total = len(results)
        n_equal = sum(1 for r in results if r["outcome"] == "equal")
        n_diff = sum(1 for r in results if r["outcome"] == "different")
        n_fuel = sum(1 for r in results if r["outcome"] == "fuel_exhausted")
        avg = ms // n_total if n_total > 0 else 0
        engines = by_label.get(label, {})
        if engine == "eqsat" and "direct" in engines and engines["direct"][0] > 0:
            ratio = f"{ms / engines['direct'][0]:.1f}x"
        else:
            ratio = ""
        print(f"{label:<28} {engine:<8} {n_total:>5} {n_equal:>5} {n_diff:>5} {n_fuel:>4} {ms:>7}ms {avg:>7}ms {ratio:>6}")

    print()


if __name__ == "__main__":
    main()
