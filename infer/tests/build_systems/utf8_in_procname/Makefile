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

SOURCES = ../codetoanalyze/make/utf8_in_function_names.c

include $(TESTS_DIR)/clang.make

infer-out/report.json: $(CLANG_DEPS) $(INFERTRACEBUGS_BIN) $(SOURCES) $(HEADERS) $(MAKEFILE_LIST)
#	set non-utf8-supporting locale
	LC_ALL=C; \
	$(call silent_on_success,\
	  $(INFER_BIN) --check-duplicate-symbols $(INFER_OPTIONS) -a $(ANALYZER) -- clang $(CLANG_OPTIONS) $(SOURCES) 2>duplicates.txt)
	grep "DUPLICATE_SYMBOLS" duplicates.txt; test $$? -ne 0
#	make sure inferTraceBugs is immune to UTF-8
	$(call silent_on_success, $(INFERTRACEBUGS_BIN) --max-level max --select 0)
	$(call silent_on_success, $(INFERTRACEBUGS_BIN) --html)
	[ -f infer-out/report.html/index.html ]
#	run again to check that infer manages to delete the results directory
	LC_ALL=C; \
	$(call silent_on_success,\
	  $(INFER_BIN) --check-duplicate-symbols $(INFER_OPTIONS) -a $(ANALYZER) -- clang $(CLANG_OPTIONS) $(SOURCES) 2>duplicates.txt)
	grep "DUPLICATE_SYMBOLS" duplicates.txt; test $$? -ne 0
