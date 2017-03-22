# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

# Targets that must be defined: CURRENT_REPORT and PREVIOUS_REPORT
# Optional variables: DIFFERENTIAL_ARGS, CLEAN_EXTRA

ROOT_DIR = $(TESTS_DIR)/../..
include $(ROOT_DIR)/Makefile.config

INFER_OUT = infer-out
DIFFERENTIAL_REPORT = $(INFER_OUT)/differential/introduced.json
EXPECTED_TEST_OUTPUT = introduced.exp.test
INFERPRINT_ISSUES_FIELDS = \
	"bug_type,file,procedure,line_offset,procedure_id,procedure_id_without_crc"

CURRENT_DIR = infer-current
PREVIOUS_DIR = infer-previous
CURRENT_REPORT = $(CURRENT_DIR)/report.json
PREVIOUS_REPORT = $(PREVIOUS_DIR)/report.json

default: analyze

# the following dependency is to guarantee that the computation of
# PREVIOUS_REPORT and CURRENT_REPORT will be serialized
$(PREVIOUS_REPORT): $(CURRENT_REPORT)

.PHONY: analyze
analyze: $(CURRENT_REPORT) $(PREVIOUS_REPORT)

$(DIFFERENTIAL_REPORT): $(CURRENT_REPORT) $(PREVIOUS_REPORT)
	$(INFER_BIN) -o $(INFER_OUT) --project-root $(CURDIR) --diff \
		--report-current $(CURRENT_REPORT) --report-previous $(PREVIOUS_REPORT) \
		$(DIFFERENTIAL_ARGS)

$(EXPECTED_TEST_OUTPUT): $(DIFFERENTIAL_REPORT) $(INFERPRINT_BIN)
	$(INFERPRINT_BIN) \
		--issues-fields $(INFERPRINT_ISSUES_FIELDS) \
		--from-json-report $(INFER_OUT)/differential/introduced.json \
		--issues-tests introduced.exp.test
	$(INFERPRINT_BIN) \
		--issues-fields $(INFERPRINT_ISSUES_FIELDS) \
		--from-json-report $(INFER_OUT)/differential/fixed.json \
		--issues-tests fixed.exp.test
	$(INFERPRINT_BIN) \
		--issues-fields $(INFERPRINT_ISSUES_FIELDS) \
		--from-json-report $(INFER_OUT)/differential/preexisting.json \
		--issues-tests preexisting.exp.test

.PHONY: print
print: $(EXPECTED_TEST_OUTPUT)

.PHONY: test
test: print
	diff -u introduced.exp introduced.exp.test
	diff -u fixed.exp fixed.exp.test
	diff -u preexisting.exp preexisting.exp.test

.PHONY: replace
replace: $(EXPECTED_TEST_OUTPUT)
	cp introduced.exp.test introduced.exp
	cp fixed.exp.test fixed.exp
	cp preexisting.exp.test preexisting.exp

.PHONY: clean
clean:
	$(REMOVE_DIR) *.exp.test $(INFER_OUT) $(CURRENT_DIR) $(PREVIOUS_DIR) \
		$(CLEAN_EXTRA)
