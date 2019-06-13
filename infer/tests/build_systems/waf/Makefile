# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

CUR_DIR = $(shell pwd)
TESTS_DIR = ../..

CLANG_OPTIONS = -c
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard ../codetoanalyze/make/*.c)

include $(TESTS_DIR)/clang.make

infer-out/report.json: $(CLANG_DEPS) $(SOURCES)
	$(QUIET)$(call silent_on_success,Testing Waf clang integration,\
	  $(TESTLOCK) sh -c "$(MAKE) -C ../codetoanalyze/make clean && \
	    cd ../codetoanalyze/make && $(INFER_BIN) --dump-duplicate-symbols \
		  --results-dir $(CUR_DIR)/$(@D) -- ./waf")
