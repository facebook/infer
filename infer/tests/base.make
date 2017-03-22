# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

ROOT_DIR = $(TESTS_DIR)/../..

# The relative path from infer/tests/ to the directory containing the current Makefile. This is
# computed in a hacky way and might not always be a relative path, so only use this for cosmetic
# reasons.
TEST_REL_DIR = $(patsubst $(abspath $(TESTS_DIR))/%,%,$(abspath $(CURDIR)))

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
	@cd $(TESTS_DIR) && \
	diff -u $(TEST_REL_DIR)/issues.exp $(TEST_REL_DIR)/issues.exp.test$(TEST_SUFFIX)

.PHONY: print
replace: issues.exp.test$(TEST_SUFFIX)
	cp $< issues.exp

.PHONY: clean
clean:
	$(REMOVE_DIR) codetoanalyze com issues.exp.test$(TEST_SUFFIX) infer-out$(TEST_SUFFIX) \
	  $(OBJECTS) $(CLEAN_EXTRA)
