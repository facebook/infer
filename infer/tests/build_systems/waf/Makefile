# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

CUR_DIR = $(shell pwd)
TESTS_DIR = ../..

ANALYZER = infer

CLANG_OPTIONS = -c
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard ../codetoanalyze/make/*.c)

include $(TESTS_DIR)/clang.make

infer-out/report.json: $(CLANG_DEPS) $(SOURCES)
	$(QUIET)$(MAKE) -C ../codetoanalyze/make clean
	$(QUIET)cd ../codetoanalyze/make && \
	$(call silent_on_success,Testing Waf clang integration,\
	  $(INFER_BIN) --dump-duplicate-symbols --results-dir $(CUR_DIR)/$(@D) -a $(ANALYZER) -- \
	    ./waf)
