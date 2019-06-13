# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

CLANG_OPTIONS = -c
INFER_OPTIONS = --project-root ../codetoanalyze
INFERPRINT_OPTIONS = --issues-tests

SOURCES1 = ../codetoanalyze/hello.c
SOURCES2 = ../codetoanalyze/hello2.c
SOURCES3 = ../codetoanalyze/hello3.c
SOURCES = $(SOURCES1) $(SOURCES2) $(SOURCES3)

include $(TESTS_DIR)/clang.make

infer-out/report.json: $(CLANG_DEPS) $(SOURCES) $(HEADERS)
	$(QUIET)$(call silent_on_success,Testing reactive mode: capture 1/3,\
	  $(INFER_BIN) $(INFER_OPTIONS) capture -- \
	    clang $(CLANG_OPTIONS) $(SOURCES1))
	$(QUIET)$(call silent_on_success,Testing reactive mode: capture 2/3,\
	  $(INFER_BIN) $(INFER_OPTIONS) capture --reactive --continue -- \
	    clang $(CLANG_OPTIONS) $(SOURCES2))
	$(QUIET)$(call silent_on_success,Testing reactive mode: capture 3/3,\
	  $(INFER_BIN) $(INFER_OPTIONS) capture --reactive --continue -- \
	    clang $(CLANG_OPTIONS) $(SOURCES3))
	$(QUIET)$(call silent_on_success,Testing reactive mode: analysis,\
	  $(INFER_BIN) analyze --dump-duplicate-symbols $(INFER_OPTIONS))
	$(QUIET)$(call check_no_duplicates,infer-out/duplicates.txt)
