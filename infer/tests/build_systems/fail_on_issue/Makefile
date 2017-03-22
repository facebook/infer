# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..

ANALYZER = infer

CODETOANALYZE_DIR = ../codetoanalyze

CLANG_OPTIONS = -c
INFER_OPTIONS = --project-root $(CODETOANALYZE_DIR) --fail-on-issue
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(CODETOANALYZE_DIR)/hello.c
OBJECTS = $(foreach source,$(SOURCES),$(basename $(source)).o)

ROOT_DIR = $(TESTS_DIR)/../..

include $(ROOT_DIR)/Makefile.config

# see base.make
TEST_REL_DIR = $(patsubst $(abspath $(TESTS_DIR))/%,%,$(abspath $(CURDIR)))

default: compile

issues.exp.test: $(CLANG_DEPS) $(SOURCES)
	$(call silent_on_success,\
	  ($(INFER_BIN) --fail-on-issue -- clang $(CLANG_OPTIONS) $(SOURCES); \
	   echo "infer exit code: $$?" > $@))

.PHONY: compile
compile: $(OBJECTS)

.PHONY: print
print: issues.exp.test

.PHONY: test
test: issues.exp.test
	@cd $(TESTS_DIR) && \
	diff -u $(TEST_REL_DIR)/issues.exp $(TEST_REL_DIR)/issues.exp.test

.PHONY: replace
replace: issues.exp.test
	cp $< issues.exp

.PHONY: clean
clean:
	rm -rf issues.exp.test infer-out $(OBJECTS)
