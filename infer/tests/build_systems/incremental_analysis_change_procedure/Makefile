# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

MODIFIED_FILES_FILE = changed_files.txt
SOURCES = $(wildcard src/*.c src/*.h)
INFER_OPTIONS = --changed-files-index $(MODIFIED_FILES_FILE) \
  --incremental-analysis \
  --debug-level-analysis 1

include $(TESTS_DIR)/differential.make

.PHONY: compare_cg
compare_cg: $(PREVIOUS_REPORT)
	$(QUIET)$(call check_no_diff,\
	  reverse_analysis_callgraph.dot,\
	  reverse_analysis_callgraph.dot.test)

replace_cg: $(PREVIOUS_REPORT)
	$(COPY) reverse_analysis_callgraph.dot.test reverse_analysis_callgraph.dot

test: compare_cg
replace: replace_cg

$(CURRENT_REPORT): $(INFER_CLANG_DEPS) $(SOURCES)
	$(QUIET)$(INFER_BIN) -o $(CURRENT_DIR) -- clang -c src/*.c

$(PREVIOUS_REPORT): $(INFER_CLANG_DEPS) $(SOURCES) $(CURRENT_REPORT)
	$(QUIET)$(REMOVE_DIR) $(PREVIOUS_DIR)
	$(QUIET)$(COPY) -r $(CURRENT_DIR) $(PREVIOUS_DIR)
	$(QUIET)$(REMOVE) $@
	$(QUIET)$(INFER_BIN) -o $(PREVIOUS_DIR) $(INFER_OPTIONS) -- clang -c src/*.c 
	$(QUIET)$(COPY) $(PREVIOUS_DIR)/reverse_analysis_callgraph.dot \
	        reverse_analysis_callgraph.dot.test
