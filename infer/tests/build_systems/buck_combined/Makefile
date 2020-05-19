# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..
ROOT_DIR = $(TESTS_DIR)/../..

BUCK_TARGET = //clang:hello //java:hello
SOURCES = clang/hello.c clang/hello2.c
OBJECTS = buck-out/gen/clang/hello\#compile-hello.c.o1f717d69,default/hello.c.o
INFER_OPTIONS = --report-custom-error --developer-mode --no-linters --buck-combined
INFERPRINT_OPTIONS = --issues-tests
CLEAN_EXTRA = buck-out

include $(TESTS_DIR)/infer.make

$(OBJECTS): $(SOURCES)
	$(QUIET)$(call silent_on_success,Compiling Buck flavors tests,\
	$(BUCK) build --no-cache $(BUCK_TARGET))

infer-out/report.json: $(CLANG_DEPS) $(SOURCES) $(MAKEFILE_LIST)
	$(QUIET)$(REMOVE_DIR) buck-out && \
	$(call silent_on_success,Testing infer-run Buck combined genrule integration,\
	$(INFER_BIN) $(INFER_OPTIONS) run --results-dir $(CURDIR)/infer-out -- \
	  $(BUCK) build --no-cache $(BUCK_TARGET))
