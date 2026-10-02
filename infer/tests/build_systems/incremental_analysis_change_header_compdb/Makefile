# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

MODIFIED_FILES_FILE = changed_files.txt
SOURCES = $(wildcard src/*.cpp src/*.h)
CLEAN_EXTRA += compile_commands_current.json compile_commands_previous.json
# the index lists only widget.h: the files that include it are found and captured
INFER_OPTIONS = --changed-files-index $(MODIFIED_FILES_FILE) \
  --capture-includers-of-changed-headers \
  --incremental-analysis \
  --debug-level-analysis 1 \
  --jobs 1

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

compile_commands_current.json: compile_commands.json.in
	$(QUIET)sed -e 's#%pwd%#$(CURDIR)#g' -e 's#%flags%#-DCHANGED#g' $< > $@ || $(REMOVE) $@

compile_commands_previous.json: compile_commands.json.in
	$(QUIET)sed -e 's#%pwd%#$(CURDIR)#g' -e 's#%flags%##g' $< > $@ || $(REMOVE) $@

$(CURRENT_REPORT): $(INFER_CLANG_DEPS) $(SOURCES) compile_commands_current.json
	$(QUIET)$(INFER_BIN) -P -o $(CURRENT_DIR) --compilation-database compile_commands_current.json

$(PREVIOUS_REPORT): $(INFER_CLANG_DEPS) $(SOURCES) $(CURRENT_REPORT) compile_commands_previous.json
	$(QUIET)$(REMOVE_DIR) $(PREVIOUS_DIR)
	$(QUIET)$(COPY) -r $(CURRENT_DIR) $(PREVIOUS_DIR)
	$(QUIET)$(REMOVE) $@
	$(QUIET)$(call silent_on_success,Testing the incremental analysis of a changed header,\
	  $(INFER_BIN) -P -o $(PREVIOUS_DIR) $(INFER_OPTIONS) \
	    --compilation-database compile_commands_previous.json)
#	the capture processes of the compilation database also invalidate the results, and only log it
	$(QUIET)grep -o "Incremental analysis: Invalidated .*" $(PREVIOUS_DIR)/logs | cut -f1 -d"[" \
	  > invalidation_stats.exp.test
	$(QUIET)$(COPY) $(PREVIOUS_DIR)/analysis_dependency_invalidation_graph.dot \
	        analysis_dependency_invalidation_graph.dot.test
