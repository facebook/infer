#!/bin/bash
# Semdiff demo runner — runs all 6 scenarios and prints results.
# Usage: bash run_demo.sh

set -euo pipefail

DEMO_DIR="$(cd "$(dirname "$0")" && pwd)"
INFER="${INFER:-infer}"
TMPDIR=$(mktemp -d)
trap "rm -rf $TMPDIR" EXIT

EMPTY_CONFIG="$DEMO_DIR/configs/empty.py"

run_semdiff() {
    local label="$1"
    local config="$2"
    local before="$3"
    local after="$4"
    local outdir="$TMPDIR/out"
    rm -rf "$outdir"

    $INFER semdiff --quiet --no-progress-bar \
        --semdiff-configuration "$config" \
        --semdiff-previous "$before" \
        --semdiff-current "$after" \
        -o "$outdir" 2>/dev/null

    local outcome
    outcome=$(python3 -c "import json,sys; print(json.load(open(sys.argv[1]))['outcome'])" "$outdir/semdiff.json")
    printf "  %-50s => %s\n" "$label" "$outcome"
}

echo "======================================"
echo " Semdiff Configuration Demo"
echo "======================================"
echo ""

echo "--- Scenario 1: ignore() — Ignoring import changes ---"
run_semdiff "with config (ignore imports)" \
    "$DEMO_DIR/configs/01_ignore_imports.py" \
    "$DEMO_DIR/examples/01_imports/before.py" \
    "$DEMO_DIR/examples/01_imports/after.py"
run_semdiff "without rules (empty config)" \
    "$EMPTY_CONFIG" \
    "$DEMO_DIR/examples/01_imports/before.py" \
    "$DEMO_DIR/examples/01_imports/after.py"
echo ""

echo "--- Scenario 2: rewrite() — Assign ≡ AnnAssign ---"
run_semdiff "with config (rewrite + accept)" \
    "$DEMO_DIR/configs/02_rewrite_assign.py" \
    "$DEMO_DIR/examples/02_assign_to_annassign/before.py" \
    "$DEMO_DIR/examples/02_assign_to_annassign/after.py"
run_semdiff "without rules (empty config)" \
    "$EMPTY_CONFIG" \
    "$DEMO_DIR/examples/02_assign_to_annassign/before.py" \
    "$DEMO_DIR/examples/02_assign_to_annassign/after.py"
echo ""

echo "======================================"
echo " Done."
echo "======================================"
