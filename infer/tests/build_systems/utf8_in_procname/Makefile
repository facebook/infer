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

infer-out/report.json: $(CLANG_DEPS) $(SOURCES) $(HEADERS) $(MAKEFILE_LIST)
#	set non-utf8-supporting locale
	$(QUIET)LC_ALL=C; \
	$(call silent_on_success,Testing Infer is immune to UTF-8 in procnames,\
	  $(INFER_BIN) --dump-duplicate-symbols $(INFER_OPTIONS) -a $(ANALYZER) -- clang $(CLANG_OPTIONS) $(SOURCES))
	$(QUIET)$(call check_no_duplicates,infer-out/duplicates.txt)
#	make sure inferTraceBugs is immune to UTF-8
	$(QUIET)$(call silent_on_success,Testing inferTraceBugs is immune to UTF-8 in procnames,\
	$(INFER_BIN) explore --select 0)
	$(QUIET)$(call silent_on_success,Testing inferTraceBugs --html is immune to UTF-8 in procnames,\
	$(INFER_BIN) explore --html)
	$(QUIET)[ -f infer-out/report.html/index.html ]
#	run again to check that infer manages to delete the results directory
	$(QUIET)LC_ALL=C; \
	$(call silent_on_success,Testing Infer rerun is immune to UTF-8 in procnames,\
	  $(INFER_BIN) --dump-duplicate-symbols $(INFER_OPTIONS) -a $(ANALYZER) -- clang $(CLANG_OPTIONS) $(SOURCES))
	$(QUIET)$(call check_no_duplicates,infer-out/duplicates.txt)
