# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

INFERPRINT_OPTIONS = --issues-tests

REBAR3_DIRS = erl_hi

CLEAN_EXTRA = \
  $(REBAR3_DIRS:%=../codetoanalyze/rebar3/%/target) \
  $(REBAR3_DIRS:%=../codetoanalyze/rebar3/%/com) \
  $(REBAR3_DIRS:%=infer-out-%) \
  $(REBAR3_DIRS:%=issues-%.exp.test)

include $(TESTS_DIR)/infer.make

infer-out/report.json:
	$(QUIET)$(MKDIR_P) $(@D)
	$(QUIET)touch $@

infer-out-%/report.json: $(JAVA_DEPS) $(SOURCES)
	$(QUIET)cd ../codetoanalyze/rebar3/$* && \
	$(call silent_on_success,Testing rebar3 Erlang integration: $*,\
	  $(INFER_BIN) --results-dir $(CURDIR)/$(@D) \
	    --project-root $(CURDIR)/$(TESTS_DIR) -- \
	    $(REBAR3) compile)

issues-%.exp.test: infer-out-%/report.json
	$(QUIET)$(INFER_BIN) report -q $(INFERPRINT_OPTIONS) $@ -o $(<D)

issues.exp.test: $(foreach r3dir,$(REBAR3_DIRS),issues-$(r3dir).exp.test)
#	erase the contents of the file
	$(QUIET): > $@
#	remember the file name so it's easier to know which bug is from where
	$(QUIET)for r3dir in $(REBAR3_DIRS); do \
	  echo "-- $$r3dir" >> $@; \
	  cat issues-$$r3dir.exp.test >> $@; \
	done
