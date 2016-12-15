# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

ROOT_DIR = $(TESTS_DIR)/../..

include $(ROOT_DIR)/Makefile.config

default: compile

issues.exp.test$(TEST_SUFFIX): infer-out$(TEST_SUFFIX)/report.json $(INFERPRINT_BIN)
	$(INFERPRINT_BIN) -q -a $(ANALYZER) $(INFERPRINT_OPTIONS) $@ --from-json-report $<

.PHONY: compile
compile: $(OBJECTS)

.PHONY: analyze
analyze: infer-out$(TEST_SUFFIX)/report.json

.PHONY: print
print: issues.exp.test$(TEST_SUFFIX)

.PHONY: test
test: issues.exp.test$(TEST_SUFFIX)
	diff -u issues.exp issues.exp.test$(TEST_SUFFIX)

.PHONY: clean
clean:
	$(REMOVE_DIR) codetoanalyze issues.exp.test$(TEST_SUFFIX) infer-out$(TEST_SUFFIX) \
	  $(OBJECTS) $(CLEAN_EXTRA)
