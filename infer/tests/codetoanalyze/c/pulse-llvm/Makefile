
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

CLANG_OPTIONS = -c -g
INFER_OPTIONS = \
  --pulse-only --debug-exceptions --project-root $(TESTS_DIR) \
  --pulse-report-issues-for-tests --jobs 1 --pulse-report-assert --enable-issue-type PULSE_ASSERTION_ERROR \

INFERPRINT_OPTIONS = --issues-tests

SOURCE = $(wildcard ../llvm-frontend/base.c )

ROOT_DIR = $(TESTS_DIR)/../..

CLEAN_EXTRA += duplicates.txt
CLEAN_EXTRA += $(foreach source,$(filter %.c ,$(SOURCE)),$(basename $(source)).o)

include $(TESTS_DIR)/infer.make
include $(TESTS_DIR)/clang-base.make

# Generate a fixed report.json in a unique infer-out folder for each source file

infer-out$(TEST_SUFFIX)/report.json: $(CLANG_DEPS) $(SOURCE) $(HEADERS) $(TESTS_DIR)/.inferconfig $(MAKEFILE_LIST)
	$(QUIET)$(call silent_on_success,Testing infer/clang in $(TEST_REL_DIR)$(TEST_SUB_TITLE),\
	  $(INFER_BIN) --results-dir $(@D) --bitcode-capture --dump-duplicate-symbols \
	    $(INFER_OPTIONS) -- \
	    $(CLANG_PREFIX)/bin/clang $(CLANG_OPTIONS) $(SOURCE))
