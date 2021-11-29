# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# E2E test for differential of costs

TESTS_DIR = ../..
SOURCES = src/DiffBlock.current.m src/DiffBlock.previous.m
COPIED = src/DiffBlock.m
CLEAN_EXTRA = $(COPIED) $(COPIED:src/%.m=%.o)
CLANG_OPTIONS = -c $(OBJC_CLANG_OPTIONS)
include $(TESTS_DIR)/differential.make
include $(TESTS_DIR)/objc.make
INFERPRINT_ISSUES_FIELDS = \
	"bug_type,bucket,file,procedure,line_offset,bug_trace"

$(CURRENT_REPORT) $(PREVIOUS_REPORT): $(OBJC_DEPS)

$(CURRENT_REPORT):
	$(QUIET)$(COPY) src/DiffBlock.current.m src/DiffBlock.m
	$(QUIET)$(call silent_on_success,Testing Cost Differential: current,\
	$(INFER_BIN)  --no-filtering --cost-only --no-cost-suppress-func-ptr -o $(CURRENT_DIR) \
	  -- clang $(CLANG_OPTIONS) $(COPIED))

$(PREVIOUS_REPORT):
	$(QUIET)$(COPY) src/DiffBlock.previous.m src/DiffBlock.m
	$(QUIET)$(call silent_on_success,Testing Cost Differential: previous,\
	$(INFER_BIN) --debug --no-filtering --cost-only --no-cost-suppress-func-ptr -o $(PREVIOUS_DIR) \
	  -- clang $(CLANG_OPTIONS) $(COPIED))
