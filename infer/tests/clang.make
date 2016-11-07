# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

ROOT_DIR = $(TESTS_DIR)/../..
include $(ROOT_DIR)/Makefile.config

INFERPRINT_OPTIONS = --issues-tests

OBJECTS = $(foreach source,$(SOURCES),$(basename $(source)).o)

CLEAN_EXTRA =

default: compile

infer-out/report.json: $(CLANG_DEPS) $(SOURCES)
	$(call silent_on_success,\
	  $(INFER_BIN) --check-duplicate-symbols $(INFER_OPTIONS) -a $(ANALYZER) -- clang $(CLANG_OPTIONS) $(SOURCES) 2>duplicates.txt)
	grep "DUPLICATE_SYMBOLS" duplicates.txt; test $$? -ne 0

issues.exp.test: infer-out/report.json $(INFERPRINT_BIN)
	$(INFERPRINT_BIN) -q -a $(ANALYZER) $(INFERPRINT_OPTIONS) $@ --from-json-report $<
	LC_ALL=C sort -t, -k1,1 -k2,2 -k3n,3 -o $@ $@

$(OBJECTS): $(SOURCES)
	clang $(CLANG_OPTIONS) $(SOURCES)

.PHONY: compile
compile: $(OBJECTS)

.PHONY: analyze
analyze: infer-out/report.json

.PHONY: print
print: issues.exp.test

.PHONY: test
test: issues.exp.test
	diff -u issues.exp issues.exp.test

.PHONY: clean
clean:
	$(REMOVE_DIR) duplicates.txt issues.exp.test infer-out $(OBJECTS) $(CLEAN_EXTRA)
