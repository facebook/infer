# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..
ROOT_DIR = $(TESTS_DIR)/../..

ANALYZER = checkers
BUCK_TARGET = //src:hello @buck_target.txt
SOURCES = $(wildcard src/*.c) $(wildcard src/subtarget1/*.c) $(wildcard src/subtarget2/*.c)
OBJECTS = buck-out/gen/src/hello\#compile-hello.c.o1f717d69,default/hello.c.o
INFER_OPTIONS = --report-custom-error --developer-mode --project-root $(TESTS_DIR) --no-linters
INFERPRINT_OPTIONS = --project-root $(TESTS_DIR) --issues-tests
CLEAN_EXTRA = buck-out

include $(TESTS_DIR)/infer.make

$(OBJECTS): $(JAVA_SOURCE_FILES)
	$(QUIET)$(call silent_on_success,Compiling Buck flavors tests,\
	$(BUCK) build --no-cache $(BUCK_TARGET))

infer-out/report.json: $(CLANG_DEPS) $(SOURCES) $(MAKEFILE_LIST)
	$(QUIET)$(REMOVE_DIR) buck-out && \
	$(call silent_on_success,Testing Buck flavors integration,\
	$(INFER_BIN) $(INFER_OPTIONS) capture --flavors --results-dir $(CURDIR)/infer-out \
	  @blacklist.txt --buck-blacklist '.*src/hello2\.c' \
	  -- $(BUCK) build --no-cache $(BUCK_TARGET) &&\
	$(INFER_BIN) $(INFER_OPTIONS) --merge analyze)
