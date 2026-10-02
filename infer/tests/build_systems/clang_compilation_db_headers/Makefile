# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

INFER_OPTIONS = --report-custom-error --developer-mode --project-root $(CURDIR)
SOURCES = $(wildcard src/*.cpp lib/src/*.cpp)
HEADERS = $(wildcard include/*.h include/*.ipp src/*.h)
INDEXES = index.txt index-partial.txt index-suffix.txt index-same-name.txt
CLEAN_EXTRA = compile_commands.json infer-out-index infer-out-partial infer-out-analyze \
  infer-out-suffix infer-out-same-name issues.exp.test.index issues.exp.test.partial \
  issues.exp.test.analyze issues.exp.test.suffix issues.exp.test.same-name
INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/infer.make

compile_commands.json: compile_commands.json.in
	$(QUIET)sed -e 's#%pwd%#$(CURDIR)#g' $< > $@ || $(REMOVE) $@

DEPS = compile_commands.json $(INDEXES) $(SOURCES) $(HEADERS) $(CLANG_DEPS)

infer-out/report.json:
#	make this target empty - since this test defines issues.exp.test directly

# headers have no entry in the compilation database: nothing is captured, with a warning
infer-out-index/report.json: $(DEPS)
	$(QUIET)$(call silent_on_success,Testing changed headers with a compilation database,\
	  $(INFER_BIN) $(INFER_OPTIONS) -o $(@D) \
	    --changed-files-index index.txt --compilation-database compile_commands.json && \
	  grep -q "Headers are captured through the source files that include them" $(@D)/logs)

# the files that include widget.h are not in the index, so they are not captured, with a warning
infer-out-partial/report.json: $(DEPS)
	$(QUIET)$(call silent_on_success,Testing a changed header without its includers,\
	  $(INFER_BIN) $(INFER_OPTIONS) -o $(@D) \
	    --changed-files-index index-partial.txt --compilation-database compile_commands.json && \
	  grep -q "Changed headers that define no captured procedure: include/widget.h" $(@D)/logs)

infer-out-analyze/report.json: $(DEPS)
	$(QUIET)$(call silent_on_success,Testing changed headers with an existing capture,\
	  $(INFER_BIN) capture $(INFER_OPTIONS) -o $(@D) \
	    --compilation-database compile_commands.json && \
	  $(INFER_BIN) analyze $(INFER_OPTIONS) -o $(@D) --changed-files-index index.txt)

infer-out-suffix/report.json: $(DEPS)
	$(QUIET)$(call silent_on_success,Testing changed headers with --suffix-match-changed-files,\
	  $(INFER_BIN) capture $(INFER_OPTIONS) -o $(@D) \
	    --compilation-database compile_commands.json && \
	  $(INFER_BIN) analyze $(INFER_OPTIONS) -o $(@D) --changed-files-index index-suffix.txt \
	    --suffix-match-changed-files)

infer-out-same-name/report.json: $(DEPS)
	$(QUIET)$(call silent_on_success,Testing a changed header with a source file of the same name,\
	  $(INFER_BIN) capture $(INFER_OPTIONS) -o $(@D) \
	    --compilation-database compile_commands.json && \
	  $(INFER_BIN) analyze $(INFER_OPTIONS) -o $(@D) --changed-files-index index-same-name.txt)

issues.exp.test: infer-out-index/report.json infer-out-partial/report.json \
                 infer-out-analyze/report.json infer-out-suffix/report.json \
                 infer-out-same-name/report.json
	$(QUIET)$(INFER_BIN) report -q $(INFERPRINT_OPTIONS) $@.index -o infer-out-index
	$(QUIET)$(INFER_BIN) report -q $(INFERPRINT_OPTIONS) $@.partial -o infer-out-partial
	$(QUIET)$(INFER_BIN) report -q $(INFERPRINT_OPTIONS) $@.analyze -o infer-out-analyze
	$(QUIET)$(INFER_BIN) report -q $(INFERPRINT_OPTIONS) $@.suffix -o infer-out-suffix
	$(QUIET)$(INFER_BIN) report -q $(INFERPRINT_OPTIONS) $@.same-name -o infer-out-same-name
	$(QUIET)cat $@.index $@.partial $@.analyze $@.suffix $@.same-name > $@
