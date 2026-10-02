# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

# --skip-analysis-in-path is matched against paths relative to the project root: the first regex
# matches the absolute path of every source file of this test but none of their relative paths, so
# it must not skip anything; the second regex must skip skipped/skipped.c
INFER_OPTIONS = --project-root $(CURDIR) \
  --skip-analysis-in-path '.*/clang_skip_analysis_in_path/.*' --skip-analysis-in-path 'skipped/'
SOURCES = src/not_skipped.c skipped/skipped.c
INFER_OUT = infer-out-clang
CLEAN_EXTRA = compile_commands.json infer-out-cdb issues.exp.test.cdb issues.exp.test.clang *.o
INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/infer.make

compile_commands.json: compile_commands.json.in
	$(QUIET)sed -e 's#%pwd%#$(CURDIR)#g' $< > $@ || $(REMOVE) $@

infer-out-clang/report.json: $(CLANG_DEPS) $(SOURCES) $(MAKEFILE_LIST)
	$(QUIET)$(call silent_on_success,Testing --skip-analysis-in-path with clang,\
	  $(INFER_BIN) $(INFER_OPTIONS) -o $(@D) --dump-duplicate-symbols -- clang -c $(SOURCES))

infer-out-cdb/report.json: compile_commands.json $(CLANG_DEPS) $(SOURCES) $(MAKEFILE_LIST)
	$(QUIET)$(call silent_on_success,Testing --skip-analysis-in-path with a compilation database,\
	  $(INFER_BIN) $(INFER_OPTIONS) -o $(@D) --skip-analysis-in-path-skips-compilation \
	    --compilation-database $<)

issues.exp.test: infer-out-clang/report.json infer-out-cdb/report.json
	$(QUIET)$(INFER_BIN) report -q $(INFERPRINT_OPTIONS) $@.clang -o infer-out-clang
	$(QUIET)$(INFER_BIN) report -q $(INFERPRINT_OPTIONS) $@.cdb -o infer-out-cdb
	$(QUIET)cat $@.clang $@.cdb > $@
