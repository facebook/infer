# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..
ROOT_DIR = $(TESTS_DIR)/../..

ANALYZER = infer
BUCK_TARGET = //src:hello
SOURCES = $(wildcard src/hello.c)
OBJECTS = buck-out/gen/src/hello\#compile-hello.c.o1f717d69,default/hello.c.o
INFER_OPTIONS = --report-custom-error --developer-mode --project-root $(TESTS_DIR) --no-keep-going
INFERPRINT_OPTIONS = --project-root $(TESTS_DIR) --issues-tests
CLEAN_EXTRA = buck-out

include $(TESTS_DIR)/infer.make

# Buck passes -a capture to infer
export INFER_STRICT_MODE=0

$(OBJECTS): $(JAVA_SOURCE_FILES)
	$(QUIET)$(call silent_on_success,Compiling Buck flavors tests,\
	NO_BUCKD=1 $(BUCK) build --no-cache $(BUCK_TARGET))

infer-out/report.json: $(CLANG_DEPS) $(SOURCES) $(MAKEFILE_LIST)
	$(QUIET)$(REMOVE_DIR) buck-out && \
	$(call silent_on_success,Testing infer-run Buck flavors integration,\
	NO_BUCKD=1 \
	$(INFER_BIN) $(INFER_OPTIONS) run --flavors --results-dir $(CURDIR)/infer-out -- \
	  $(BUCK) build --no-cache $(BUCK_TARGET))
