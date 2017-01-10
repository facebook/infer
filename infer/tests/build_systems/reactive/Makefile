# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..

ANALYZER = infer

CLANG_OPTIONS = -c
INFER_OPTIONS = --project-root ../codetoanalyze
INFERPRINT_OPTIONS = --issues-tests

SOURCES1 = ../codetoanalyze/hello.c
SOURCES2 = ../codetoanalyze/hello2.c
SOURCES3 = ../codetoanalyze/hello3.c
SOURCES = $(SOURCES1) $(SOURCES2) $(SOURCES3)

include $(TESTS_DIR)/clang.make

infer-out/report.json: $(CLANG_DEPS) $(SOURCES) $(HEADERS)
	$(call silent_on_success,\
	  $(INFER_BIN) $(INFER_OPTIONS) -a capture -- \
	    clang $(CLANG_OPTIONS) $(SOURCES1))
	$(call silent_on_success,\
	  $(INFER_BIN) $(INFER_OPTIONS) -a capture --reactive --continue -- \
	    clang $(CLANG_OPTIONS) $(SOURCES2))
	$(call silent_on_success,\
	  $(INFER_BIN) $(INFER_OPTIONS) -a capture --reactive --continue -- \
	    clang $(CLANG_OPTIONS) $(SOURCES3))
	$(call silent_on_success,\
	  $(INFER_BIN) --check-duplicate-symbols $(INFER_OPTIONS) -a $(ANALYZER) -- analyze 2>duplicates.txt)
	grep "DUPLICATE_SYMBOLS" duplicates.txt; test $$? -ne 0
