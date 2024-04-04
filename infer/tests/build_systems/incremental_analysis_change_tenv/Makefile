# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

MODIFIED_FILES_FILE = changed_files.txt
SOURCES = $(wildcard src/*.c src/*.h)
CLEAN_EXTRA += $(patsubst src/%.c,%.o,$(wildcard src/*.c))

INFER_OPTIONS = --changed-files-index $(MODIFIED_FILES_FILE) \
  --incremental-analysis \
  --debug-level-analysis 1

include $(TESTS_DIR)/differential.make

.PHONY: compare_cg
compare_cg: $(PREVIOUS_REPORT)
	$(QUIET)$(call check_no_diff,\
	  analysis_dependency_invalidation_graph.dot,\
	  analysis_dependency_invalidation_graph.dot.test)

.PHONY: compare_invalidation_stats
compare_invalidation_stats: $(PREVIOUS_REPORT)
	$(QUIET)$(call check_no_diff,\
	invalidation_stats.exp,\
	invalidation_stats.exp.test)

replace_cg: $(PREVIOUS_REPORT)
	$(COPY) analysis_dependency_invalidation_graph.dot.test analysis_dependency_invalidation_graph.dot

replace_invalidation_stats: $(PREVIOUS_REPORT)
	$(COPY) invalidation_stats.exp.test invalidation_stats.exp

test: compare_cg compare_invalidation_stats
replace: replace_cg replace_invalidation_stats

$(CURRENT_REPORT): $(INFER_CLANG_DEPS) $(SOURCES)
	$(QUIET)$(INFER_BIN) -P -o $(CURRENT_DIR) -- clang -c src/*.c

$(PREVIOUS_REPORT): $(INFER_CLANG_DEPS) $(SOURCES) $(CURRENT_REPORT)
	$(QUIET)$(REMOVE_DIR) $(PREVIOUS_DIR)
	$(QUIET)$(COPY) -r $(CURRENT_DIR) $(PREVIOUS_DIR)
	$(QUIET)$(REMOVE) $@
	$(QUIET)$(INFER_BIN) -P -o $(PREVIOUS_DIR) $(INFER_OPTIONS) -- clang -c src/*.c 2>&1 \
		| grep "Invalidated" | cut -f1 -d"[" > invalidation_stats.exp.test
	$(QUIET)$(COPY) $(PREVIOUS_DIR)/analysis_dependency_invalidation_graph.dot \
	        analysis_dependency_invalidation_graph.dot.test
