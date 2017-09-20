# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..

ANALYZER = infer

CLANG_OPTIONS = -c
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard ../codetoanalyze/make/*.c)

include $(TESTS_DIR)/clang.make

infer-out/report.json: $(CLANG_DEPS) $(SOURCES) $(HEADERS)
	$(QUIET)$(call silent_on_success,Testing make clang and javac integration,\
	  $(INFER_BIN) --dump-duplicate-symbols --project-root $(TESTS_DIR) -a $(ANALYZER) -- \
	    make -C ../codetoanalyze/make clean all)
	$(QUIET)$(call check_no_duplicates,infer-out/duplicates.txt)
