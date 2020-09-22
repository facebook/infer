# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Targets that must be defined: CURRENT_REPORT and PREVIOUS_REPORT
# Optional variables: DIFFERENTIAL_ARGS, CLEAN_EXTRA

include $(TESTS_DIR)/base.make

INFER_OUT = infer-out
EXPECTED_TEST_OUTPUT = introduced.exp.test
INFERPRINT_ISSUES_FIELDS = \
	"bug_type,bucket,file,procedure,line_offset"

CURRENT_DIR = infer-out-current
PREVIOUS_DIR = infer-out-previous
CURRENT_REPORT = $(CURRENT_DIR)/report.json
PREVIOUS_REPORT = $(PREVIOUS_DIR)/report.json
CURRENT_COSTS = $(CURRENT_DIR)/costs-report.json
PREVIOUS_COSTS = $(PREVIOUS_DIR)/costs-report.json

default: analyze

# the following dependency is to guarantee that the computation of
# PREVIOUS_REPORT and CURRENT_REPORT will be serialized
$(PREVIOUS_REPORT): $(CURRENT_REPORT)

.PHONY: analyze
analyze: $(CURRENT_REPORT) $(PREVIOUS_REPORT)

$(CURRENT_REPORT) $(PREVIOUS_REPORT): $(INFER_BIN) $(SOURCES)

$(EXPECTED_TEST_OUTPUT): $(CURRENT_REPORT) $(PREVIOUS_REPORT) $(MODIFIED_FILES_FILE) \
                         $(INFER_BIN) $(MAKEFILE_LIST)
	$(QUIET)$(REMOVE_DIR) $(INFER_OUT)
	$(QUIET)$(call silent_on_success,Computing results difference in $(TEST_REL_DIR),\
	  $(INFER_BIN) -o $(INFER_OUT) --project-root $(CURDIR) reportdiff \
		--report-current $(CURRENT_REPORT) --report-previous $(PREVIOUS_REPORT) \
		--costs-current $(CURRENT_COSTS) --costs-previous $(PREVIOUS_COSTS) \
		$(DIFFERENTIAL_ARGS))
	$(QUIET)$(INFER_BIN) report -o $(INFER_OUT) \
		--issues-tests-fields $(INFERPRINT_ISSUES_FIELDS) \
		--from-json-report $(INFER_OUT)/differential/introduced.json \
		--issues-tests introduced.exp.test
	$(QUIET)$(INFER_BIN) report -o $(INFER_OUT) \
		--issues-tests-fields $(INFERPRINT_ISSUES_FIELDS) \
		--from-json-report $(INFER_OUT)/differential/fixed.json \
		--issues-tests fixed.exp.test
	$(QUIET)$(INFER_BIN) report -o $(INFER_OUT) \
		--issues-tests-fields $(INFERPRINT_ISSUES_FIELDS) \
		--from-json-report $(INFER_OUT)/differential/preexisting.json \
		--issues-tests preexisting.exp.test
	$(QUIET)$(COPY) $(INFER_OUT)/differential/costs_summary.json \
		costs_summary.json.exp.test

.PHONY: print
print: $(EXPECTED_TEST_OUTPUT)

.PHONY: test
test: print
	$(QUIET)$(call check_no_diff,introduced.exp,introduced.exp.test)
	$(QUIET)$(call check_no_diff,fixed.exp,fixed.exp.test)
	$(QUIET)$(call check_no_diff,preexisting.exp,preexisting.exp.test)
	$(QUIET)$(call check_no_diff,costs_summary.json.exp,costs_summary.json.exp.test)

.PHONY: replace
replace: $(EXPECTED_TEST_OUTPUT)
	cp introduced.exp.test introduced.exp
	cp fixed.exp.test fixed.exp
	cp preexisting.exp.test preexisting.exp
	cp costs_summary.json.exp.test costs_summary.json.exp

.PHONY: clean
clean:
	$(REMOVE_DIR) *.exp.test $(INFER_OUT) $(CURRENT_DIR) $(PREVIOUS_DIR) \
		$(CLEAN_EXTRA)
