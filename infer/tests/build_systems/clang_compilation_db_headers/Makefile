# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

INFER_OPTIONS = --report-custom-error --developer-mode --project-root $(CURDIR)
SOURCES = $(wildcard src/*.c src/*.cpp lib/src/*.cpp)
HEADERS = $(wildcard include/*.h include/*.ipp src/*.h)
INDEXES = $(wildcard index*.txt)
# the runs whose issues are compared with issues.exp, in this order
RUNS = index partial includers spellings symlink suffix-includers broken analyze suffix same-name
CLEAN_EXTRA = compile_commands.json compile_commands_broken.json linkinc \
  $(foreach run,$(RUNS) deleted no-includers,infer-out-$(run)) \
  $(foreach run,$(RUNS),issues.exp.test.$(run))
INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/infer.make

compile_commands.json compile_commands_broken.json: %.json: %.json.in
	$(QUIET)sed -e 's#%pwd%#$(CURDIR)#g' $< > $@ || $(REMOVE) $@

linkinc:
	$(QUIET)ln -s include $@

DEPS = compile_commands.json compile_commands_broken.json linkinc $(INDEXES) $(SOURCES) $(HEADERS) \
  $(CLANG_DEPS)

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

# the source files that include the headers, directly or not, are captured too
infer-out-includers/report.json: $(DEPS)
	$(QUIET)$(call silent_on_success,Testing --capture-includers-of-changed-headers,\
	  $(INFER_BIN) $(INFER_OPTIONS) -o $(@D) --capture-includers-of-changed-headers \
	    --changed-files-index index.txt --compilation-database compile_commands.json)

# the entries of the index match the headers whatever their spelling
infer-out-spellings/report.json: $(DEPS)
	$(QUIET)$(call silent_on_success,Testing headers spelled with ./ .. and //,\
	  $(INFER_BIN) $(INFER_OPTIONS) -o $(@D) --capture-includers-of-changed-headers \
	    --changed-files-index index-spellings.txt --compilation-database compile_commands.json)

infer-out-symlink/report.json: $(DEPS)
	$(QUIET)$(call silent_on_success,Testing a header in a symlinked directory,\
	  $(INFER_BIN) $(INFER_OPTIONS) -o $(@D) --capture-includers-of-changed-headers \
	    --changed-files-index index-symlink.txt --compilation-database compile_commands.json)

infer-out-suffix-includers/report.json: $(DEPS)
	$(QUIET)$(call silent_on_success,Testing includers of headers with --suffix-match-changed-files,\
	  $(INFER_BIN) $(INFER_OPTIONS) -o $(@D) --capture-includers-of-changed-headers \
	    --changed-files-index index-suffix.txt --suffix-match-changed-files \
	    --compilation-database compile_commands.json)

# the includers that cannot be captured are skipped, with a warning
infer-out-broken/report.json: $(DEPS)
	$(QUIET)$(call silent_on_success,Testing includers that cannot be captured,\
	  $(INFER_BIN) $(INFER_OPTIONS) -o $(@D) --capture-includers-of-changed-headers \
	    --changed-files-index index.txt --compilation-database compile_commands.json \
	    --compilation-database compile_commands_broken.json && \
	  grep -q "Could not capture 2 source files that include the changed headers" $(@D)/logs)

infer-out-deleted/report.json: $(DEPS)
	$(QUIET)$(call silent_on_success,Testing includers of a deleted header,\
	  $(INFER_BIN) $(INFER_OPTIONS) -o $(@D) --capture-includers-of-changed-headers \
	    --changed-files-index index-deleted.txt --compilation-database compile_commands.json && \
	  grep -q "The changed headers do not exist" $(@D)/logs)

infer-out-no-includers/report.json: $(DEPS)
	$(QUIET)$(call silent_on_success,Testing a header without includers,\
	  $(INFER_BIN) $(INFER_OPTIONS) -o $(@D) --capture-includers-of-changed-headers \
	    --changed-files-index index-no-includers.txt --compilation-database compile_commands.json && \
	  grep -q "No entry of the compilation database includes the changed headers" $(@D)/logs)

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

issues.exp.test: $(foreach run,$(RUNS) deleted no-includers,infer-out-$(run)/report.json)
	$(QUIET)$(foreach run,$(RUNS),\
	  $(INFER_BIN) report -q $(INFERPRINT_OPTIONS) $@.$(run) -o infer-out-$(run) &&) true
	$(QUIET)cat $(foreach run,$(RUNS),$@.$(run)) > $@
