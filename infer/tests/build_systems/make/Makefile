# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

CLANG_OPTIONS = -c
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard ../codetoanalyze/make/*.c)

include $(TESTS_DIR)/clang.make

infer-out/report.json: $(CLANG_DEPS) $(SOURCES) $(HEADERS)
	$(QUIET)$(call silent_on_success,Testing make clang and javac integration,\
	  $(TESTLOCK) $(INFER_BIN) --dump-duplicate-symbols --project-root $(TESTS_DIR) -- \
	    make -C ../codetoanalyze/make clean all)
	$(QUIET)$(call check_no_duplicates,infer-out/duplicates.txt)
