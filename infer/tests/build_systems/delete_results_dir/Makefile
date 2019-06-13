# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Test that two consecutive analyses do not pollute each other: the expected results of running
# infer on hello.c then on hello2.c is that only the bug in hello2.c is reported.

TESTS_DIR = ../..

CODETOANALYZE_DIR = ../codetoanalyze

CLANG_OPTIONS = -c
INFER_OPTIONS = --project-root $(CODETOANALYZE_DIR)
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(CODETOANALYZE_DIR)/hello.c $(CODETOANALYZE_DIR)/hello2.c

include $(TESTS_DIR)/clang.make

infer-out/report.json: $(CLANG_DEPS) $(SOURCES)
	$(QUIET)$(call silent_on_success,Testing that infer deletes the results directory: part 1/2,\
	  $(INFER_BIN) --dump-duplicate-symbols $(INFER_OPTIONS) -- clang $(CLANG_OPTIONS) $(CODETOANALYZE_DIR)/hello.c)
	$(QUIET)$(call check_no_duplicates,infer-out/duplicates.txt)
	$(QUIET)$(call silent_on_success,Testing that infer deletes the results directory: part 2/2,\
	  $(INFER_BIN) --dump-duplicate-symbols $(INFER_OPTIONS) -- clang $(CLANG_OPTIONS) $(CODETOANALYZE_DIR)/hello2.c)
	$(QUIET)$(call check_no_duplicates,infer-out/duplicates.txt)
