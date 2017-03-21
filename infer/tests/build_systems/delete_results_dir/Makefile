# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

# Test that two consecutive analyses do not pollute each other: the expected results of running
# infer on hello.c then on hello2.c is that only the bug in hello2.c is reported.

TESTS_DIR = ../..

ANALYZER = infer

CODETOANALYZE_DIR = ../codetoanalyze

CLANG_OPTIONS = -c
INFER_OPTIONS = --project-root $(CODETOANALYZE_DIR)
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(CODETOANALYZE_DIR)/hello.c $(CODETOANALYZE_DIR)/hello2.c

include $(TESTS_DIR)/clang.make

infer-out/report.json: $(CLANG_DEPS) $(SOURCES)
	$(call silent_on_success,\
	  $(INFER_BIN) --dump-duplicate-symbols $(INFER_OPTIONS) -a $(ANALYZER) -- clang $(CLANG_OPTIONS) $(CODETOANALYZE_DIR)/hello.c)
	grep "DUPLICATE_SYMBOLS" infer-out/duplicates.txt; test $$? -ne 0
	$(call silent_on_success,\
	  $(INFER_BIN) --dump-duplicate-symbols $(INFER_OPTIONS) -a $(ANALYZER) -- clang $(CLANG_OPTIONS) $(CODETOANALYZE_DIR)/hello2.c)
	grep "DUPLICATE_SYMBOLS" infer-out/duplicates.txt; test $$? -ne 0
