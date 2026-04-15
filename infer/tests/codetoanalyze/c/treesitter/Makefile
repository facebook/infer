# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

SOURCES = $(sort $(wildcard *.c))

INFER_OPTIONS = \
  --pulse-only --debug-exceptions --project-root $(TESTS_DIR) \
  --pulse-report-issues-for-tests --no-pulse-force-continue \
  --jobs 1

INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/infer.make

infer-out/report.json: $(SOURCES) $(INFER_BIN)
	$(QUIET)$(call silent_on_success,Capturing and analyzing with tree-sitter in $(TEST_REL_DIR),\
	  $(REMOVE_DIR) infer-out && \
	  $(INFER_BIN) \
	    $(foreach src,$(SOURCES),--capture-tree-sitter $(CURDIR)/$(src)) \
	    $(INFER_OPTIONS))

# Compare tree-sitter vs clang on the full c/pulse test suite.
# Runs tree-sitter (with preprocessor) on all c/pulse source files, then
# runs clang on the same files, and diffs the results with line numbers
# stripped to focus on which issues are found in which functions.
# Usage: make compare
PULSE_DIR = ../pulse
PULSE_SOURCES = $(sort $(wildcard $(PULSE_DIR)/*.c))

.PHONY: compare
compare:
	$(QUIET)$(call silent_on_success,Capturing c/pulse with tree-sitter in $(TEST_REL_DIR),\
	  $(REMOVE_DIR) infer-out-ts && \
	  $(INFER_BIN) --results-dir infer-out-ts --keep-going \
	    $(foreach src,$(PULSE_SOURCES),--capture-tree-sitter $(CURDIR)/$(src)) \
	    $(INFER_OPTIONS))
	$(QUIET)$(INFER_BIN) report -q --results-dir infer-out-ts \
	  $(INFERPRINT_OPTIONS) issues.exp.ts
	$(QUIET)$(call silent_on_success,Capturing c/pulse with clang in $(TEST_REL_DIR),\
	  $(REMOVE_DIR) infer-out-clang && \
	  $(INFER_BIN) --results-dir infer-out-clang \
	    $(INFER_OPTIONS) \
	    -- clang -c $(PULSE_SOURCES))
	$(QUIET)$(INFER_BIN) report -q --results-dir infer-out-clang \
	  $(INFERPRINT_OPTIONS) issues.exp.clang
	$(QUIET)sed 's/, [0-9]*, /, _, /' issues.exp.clang > issues.exp.clang.norm && \
	  sed 's/, [0-9]*, /, _, /' issues.exp.ts > issues.exp.ts.norm && \
	  echo "=== Clang: $$(wc -l < issues.exp.clang) | Tree-sitter: $$(wc -l < issues.exp.ts) | Match: $$(comm -12 <(sort issues.exp.clang.norm) <(sort issues.exp.ts.norm) | wc -l) ===" && \
	  echo "=== Diff (- clang, + tree-sitter) ===" && \
	  diff -u issues.exp.clang.norm issues.exp.ts.norm || true
	$(QUIET)$(REMOVE_DIR) infer-out-ts infer-out-clang issues.exp.ts issues.exp.clang \
	  issues.exp.clang.norm issues.exp.ts.norm
