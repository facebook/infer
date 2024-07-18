# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..
include $(TESTS_DIR)/base.make

INFER_OUT = infer-out
REPORT = $(INFER_OUT)/report.json
EXPECTED_TEST_OUTPUT = qualifier.exp.test

default: analyze

.PHONY: analyze
analyze: $(REPORT)

$(REPORT): $(JAVA_DEPS) $(MAKEFILE_LIST) SimpleLeak.java
	$(QUIET)$(call silent_on_success,Running analysis,\
	  $(INFER_BIN) --pulse-only -o $(INFER_OUT) --project-root $(CURDIR) \
	    -- javac SimpleLeak.java)

$(EXPECTED_TEST_OUTPUT): $(REPORT)
	$(QUIET)$(INFER_BIN) report -o $(<D) \
		--issues-tests-fields "qualifier_contains_potential_exception_note,bug_type,bucket,file,procedure,line_offset" \
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
