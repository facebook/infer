# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..
include $(TESTS_DIR)/base.make

INFER_OUT = infer-out
REPORT = $(INFER_OUT)/report.json
EXPECTED_TEST_OUTPUT = qualifier.exp.test

default: analyze

.PHONY: analyze
analyze: $(REPORT)

$(REPORT):
	$(QUIET)$(call silent_on_success, Running analysis,\
	  $(INFER_BIN) -o $(INFER_OUT) --project-root $(CURDIR) -- javac SimpleLeak.java)

$(EXPECTED_TEST_OUTPUT): $(REPORT)
	$(QUIET)$(INFER_BIN) report \
		--issues-fields "qualifier_contains_potential_exception_note,bug_type,file,procedure,line_offset" \
		--from-json-report $(REPORT) \
		--issues-tests $(EXPECTED_TEST_OUTPUT)

.PHONY: print
print: $(EXPECTED_TEST_OUTPUT)

.PHONY: test
test: print
	$(QUIET)$(call check_no_diff,qualifier.exp,$(EXPECTED_TEST_OUTPUT))

.PHONY: replace
replace: $(EXPECTED_TEST_OUTPUT)
	cp $(EXPECTED_TEST_OUTPUT) qualifier.exp

.PHONY: clean
clean:
	$(REMOVE_DIR) *.class *.exp.test $(INFER_OUT)
