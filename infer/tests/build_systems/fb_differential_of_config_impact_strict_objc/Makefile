# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..
SOURCES = src/DiffExample.current.m src/DiffExample.previous.m src/DiffTest.current.m src/DiffTest.previous.m
COPIED = src/DiffExample.m src/DiffTest.m
CLEAN_EXTRA = $(COPIED) *.o
CLANG_OPTIONS = -c $(OBJC_CLANG_OPTIONS)
DIFFERENTIAL_ARGS = --enable-issue-type CONFIG_IMPACT
INFERPRINT_ISSUES_FIELDS = "bug_type,bucket,file,procedure,line_offset,bug_trace"

include $(TESTS_DIR)/differential-config-impact.make
include $(TESTS_DIR)/objc.make

$(CURRENT_REPORT) $(PREVIOUS_REPORT): $(OBJC_DEPS)

$(CURRENT_REPORT):
	$(QUIET)$(COPY) src/DiffExample.current.m src/DiffExample.m
	$(QUIET)$(COPY) src/DiffTest.current.m src/DiffTest.m
	$(QUIET)$(call silent_on_success,Testing ObjC Config Impact Differential: current,\
	$(INFER_BIN) --no-filtering --config-impact-analysis-only \
	  --config-impact-strict-mode -o $(CURRENT_DIR) \
	  --config-impact-test-paths "src/DiffTest.m" \
	  -- clang $(CLANG_OPTIONS) $(COPIED))

$(PREVIOUS_REPORT):
	$(QUIET)$(COPY) src/DiffExample.previous.m src/DiffExample.m
	$(QUIET)$(COPY) src/DiffTest.previous.m src/DiffTest.m
	$(QUIET)$(call silent_on_success,Testing ObjC Config Impact Differential: previous,\
	$(INFER_BIN) --no-filtering --config-impact-analysis-only \
	  --config-impact-strict-mode -o $(PREVIOUS_DIR) \
	  --config-impact-test-paths "src/DiffTest.m" \
	  -- clang $(CLANG_OPTIONS) $(COPIED))
