# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# E2E test for differential of costs

TESTS_DIR = ../..
SOURCES = src/DiffBlock.m.current src/DiffBlock.m.previous
CLANG_OPTIONS = -c $(OBJC_CLANG_OPTIONS)
include $(TESTS_DIR)/differential.make
include $(TESTS_DIR)/objc.make
INFERPRINT_ISSUES_FIELDS = \
	"bug_type,bucket,file,procedure,line_offset,bug_trace"

$(CURRENT_REPORT) $(PREVIOUS_REPORT): $(OBJC_DEPS)

$(CURRENT_REPORT):
	$(QUIET)$(COPY) src/DiffBlock.m.current src/DiffBlock.m
	$(QUIET)$(call silent_on_success,Testing Cost Differential: current,\
	$(INFER_BIN) --enable-issue-type INFINITE_EXECUTION_TIME --cost-only -o $(CURRENT_DIR) -- clang $(CLANG_OPTIONS) src/*.m)

$(PREVIOUS_REPORT):
	$(QUIET)$(COPY) src/DiffBlock.m.previous src/DiffBlock.m
	$(QUIET)$(call silent_on_success,Testing Cost Differential: previous,\
	$(INFER_BIN) --debug --enable-issue-type INFINITE_EXECUTION_TIME --cost-only -o $(PREVIOUS_DIR) -- clang $(CLANG_OPTIONS) src/*.m)
