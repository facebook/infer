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

    $INFER semdiff \
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

echo "--- Scenario 3: accept() with key — Adding annotations ---"
run_semdiff "with config (accept in annotation positions)" \
    "$DEMO_DIR/configs/03_accept_annotations.py" \
    "$DEMO_DIR/examples/03_add_annotations/before.py" \
    "$DEMO_DIR/examples/03_add_annotations/after.py"
run_semdiff "without rules (empty config)" \
    "$EMPTY_CONFIG" \
    "$DEMO_DIR/examples/03_add_annotations/before.py" \
    "$DEMO_DIR/examples/03_add_annotations/after.py"
echo ""

echo "--- Scenario 4: accept() with condition — Rejecting Any ---"
run_semdiff "with condition (reject Any annotation)" \
    "$DEMO_DIR/configs/04_accept_with_condition.py" \
    "$DEMO_DIR/examples/04_annotation_with_any/before.py" \
    "$DEMO_DIR/examples/04_annotation_with_any/after.py"
run_semdiff "without condition (accept all annotations)" \
    "$DEMO_DIR/configs/03_accept_annotations.py" \
    "$DEMO_DIR/examples/04_annotation_with_any/before.py" \
    "$DEMO_DIR/examples/04_annotation_with_any/after.py"
echo ""

echo "--- Scenario 5: Full config — Combined changes ---"
run_semdiff "with full config" \
    "$DEMO_DIR/configs/05_full.py" \
    "$DEMO_DIR/examples/05_combined/before.py" \
    "$DEMO_DIR/examples/05_combined/after.py"
run_semdiff "without rules (empty config)" \
    "$EMPTY_CONFIG" \
    "$DEMO_DIR/examples/05_combined/before.py" \
    "$DEMO_DIR/examples/05_combined/after.py"
echo ""

echo "--- Scenario 6: Catching real semantic changes ---"
run_semdiff "with full config (catches real change)" \
    "$DEMO_DIR/configs/05_full.py" \
    "$DEMO_DIR/examples/06_real_change/before.py" \
    "$DEMO_DIR/examples/06_real_change/after.py"
echo ""

echo "======================================"
echo " Done."
echo "======================================"
