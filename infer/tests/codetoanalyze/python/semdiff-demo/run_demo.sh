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
    local before="$2"
    local after="$3"
    shift 3  # remaining arguments are config files
    local outdir="$TMPDIR/out"
    rm -rf "$outdir"

    local config_args=""
    for cfg in "$@"; do
        config_args="$config_args --semdiff-configuration $cfg"
    done

    $INFER semdiff \
        $config_args \
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
    "$DEMO_DIR/examples/01_imports/before.py" \
    "$DEMO_DIR/examples/01_imports/after.py" \
    "$DEMO_DIR/configs/01_ignore_imports.py"
run_semdiff "without rules (empty config)" \
    "$DEMO_DIR/examples/01_imports/before.py" \
    "$DEMO_DIR/examples/01_imports/after.py" \
    "$EMPTY_CONFIG"
echo ""

echo "--- Scenario 2: rewrite() — Assign ≡ AnnAssign ---"
run_semdiff "with config (rewrite + accept)" \
    "$DEMO_DIR/examples/02_assign_to_annassign/before.py" \
    "$DEMO_DIR/examples/02_assign_to_annassign/after.py" \
    "$DEMO_DIR/configs/02_rewrite_assign.py"
run_semdiff "without rules (empty config)" \
    "$DEMO_DIR/examples/02_assign_to_annassign/before.py" \
    "$DEMO_DIR/examples/02_assign_to_annassign/after.py" \
    "$EMPTY_CONFIG"
echo ""

echo "--- Scenario 3: accept() with key — Adding annotations ---"
run_semdiff "with config (accept in annotation positions)" \
    "$DEMO_DIR/examples/03_add_annotations/before.py" \
    "$DEMO_DIR/examples/03_add_annotations/after.py" \
    "$DEMO_DIR/configs/03_accept_annotations.py"
run_semdiff "without rules (empty config)" \
    "$DEMO_DIR/examples/03_add_annotations/before.py" \
    "$DEMO_DIR/examples/03_add_annotations/after.py" \
    "$EMPTY_CONFIG"
echo ""

echo "--- Scenario 4: accept() with condition — Rejecting Any ---"
run_semdiff "with condition (reject Any annotation)" \
    "$DEMO_DIR/examples/04_annotation_with_any/before.py" \
    "$DEMO_DIR/examples/04_annotation_with_any/after.py" \
    "$DEMO_DIR/configs/04_accept_with_condition.py"
run_semdiff "without condition (accept all annotations)" \
    "$DEMO_DIR/examples/04_annotation_with_any/before.py" \
    "$DEMO_DIR/examples/04_annotation_with_any/after.py" \
    "$DEMO_DIR/configs/03_accept_annotations.py"
echo ""

echo "--- Scenario 5: Full config — Combined changes ---"
run_semdiff "with full config" \
    "$DEMO_DIR/examples/05_combined/before.py" \
    "$DEMO_DIR/examples/05_combined/after.py" \
    "$DEMO_DIR/configs/05_full.py"
run_semdiff "without rules (empty config)" \
    "$DEMO_DIR/examples/05_combined/before.py" \
    "$DEMO_DIR/examples/05_combined/after.py" \
    "$EMPTY_CONFIG"
echo ""

echo "--- Scenario 6: Catching real semantic changes ---"
run_semdiff "with full config (catches real change)" \
    "$DEMO_DIR/examples/06_real_change/before.py" \
    "$DEMO_DIR/examples/06_real_change/after.py" \
    "$DEMO_DIR/configs/05_full.py"
echo ""

echo "--- Scenario 7: Multiple config files — Union of rules ---"
run_semdiff "with two configs (ignore + accept)" \
    "$DEMO_DIR/examples/07_multi_config/before.py" \
    "$DEMO_DIR/examples/07_multi_config/after.py" \
    "$DEMO_DIR/configs/01_ignore_imports.py" \
    "$DEMO_DIR/configs/03_accept_annotations.py"
run_semdiff "with only ignore config (misses annotations)" \
    "$DEMO_DIR/examples/07_multi_config/before.py" \
    "$DEMO_DIR/examples/07_multi_config/after.py" \
    "$DEMO_DIR/configs/01_ignore_imports.py"
run_semdiff "with only accept config (misses imports)" \
    "$DEMO_DIR/examples/07_multi_config/before.py" \
    "$DEMO_DIR/examples/07_multi_config/after.py" \
    "$DEMO_DIR/configs/03_accept_annotations.py"
echo ""

echo "======================================"
echo " Done."
echo "======================================"
