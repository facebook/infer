# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

include $(TESTS_DIR)/base.make

# useful to print non-default analyzer
ANALYZER_STRING=$(shell if [ -n $(ANALYZER) ] && [ $(ANALYZER) != infer ]; then \
  printf ' ($(ANALYZER))'; fi)

default: compile

issues.exp.test$(TEST_SUFFIX): infer-out$(TEST_SUFFIX)/report.json $(INFERPRINT_BIN)
	$(QUIET)$(INFERPRINT_BIN) -q -a $(ANALYZER) $(INFERPRINT_OPTIONS) $@ --from-json-report $<

.PHONY: compile
compile: $(OBJECTS)

.PHONY: analyze
analyze: infer-out$(TEST_SUFFIX)/report.json

.PHONY: print
print: issues.exp.test$(TEST_SUFFIX)

.PHONY: test
test: issues.exp.test$(TEST_SUFFIX)
	$(QUIET)cd $(TESTS_DIR) && \
	$(call check_no_diff,$(TEST_REL_DIR)/issues.exp,$(TEST_REL_DIR)/issues.exp.test$(TEST_SUFFIX))

.PHONY: print
replace: issues.exp.test$(TEST_SUFFIX)
	cp $< issues.exp

.PHONY: clean
clean:
	$(REMOVE_DIR) codetoanalyze com issues.exp.test$(TEST_SUFFIX) infer-out$(TEST_SUFFIX) \
	  $(OBJECTS) $(CLEAN_EXTRA)
