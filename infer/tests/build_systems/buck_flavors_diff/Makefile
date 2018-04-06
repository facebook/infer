# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..
INFER_OUT = infer-out
SOURCES = $(SRC_DIR)/some_bugs.c $(SRC_DIR)/some_different_bugs.c
DIFFERENTIAL_ARGS = --differential-filter-files changed_files.txt

include $(TESTS_DIR)/differential.make

$(CURRENT_REPORT) $(PREVIOUS_REPORT): $(CLANG_DEPS)

BUCK_TARGET = //src:hello
INFER_OPTIONS = $(DIFFERENTIAL_ARGS) --flavors --reactive --debug-exceptions
SRC_DIR = $(CURDIR)/../codetoanalyze

.PHONY: compare_reports
compare_reports: current.exp.test previous.exp.test
	$(QUIET)$(call check_no_diff,current.exp,current.exp.test)
	$(QUIET)$(call check_no_diff,previous.exp,previous.exp.test)

.PHONY: replace_reports
replace_reports: current.exp.test previous.exp.test
	$(COPY) current.exp.test current.exp
	$(COPY) previous.exp.test previous.exp

current.exp.test: $(CURRENT_REPORT)
	$(QUIET)$(INFER_BIN) report -o $(<D) \
		--from-json-report $(CURRENT_DIR)/report.json \
		--issues-tests current.exp.test

previous.exp.test: $(PREVIOUS_REPORT)
	$(QUIET)$(INFER_BIN) report -o $(<D) \
		--from-json-report $(PREVIOUS_DIR)/report.json \
		--issues-tests previous.exp.test

test: compare_reports
print: current.exp.test previous.exp.test
replace: replace_reports

$(CURRENT_REPORT):
	$(QUIET)$(REMOVE_DIR) buck-out
	$(QUIET)$(COPY) $(SRC_DIR)/some_bugs.c src/hello.c
	$(QUIET)$(call silent_on_success,Running Buck diff analysis: current,\
	  $(INFER_BIN) $(INFER_OPTIONS) run --results-dir $(CURRENT_DIR) -- \
	  $(BUCK) build --no-cache $(BUCK_TARGET))

$(PREVIOUS_REPORT):
	$(QUIET)$(COPY) $(SRC_DIR)/some_different_bugs.c src/hello.c
	$(QUIET)$(call silent_on_success,Running Buck diff analysis: previous,\
	  $(INFER_BIN) $(INFER_OPTIONS) run --results-dir $(PREVIOUS_DIR) -- \
	  $(BUCK) build --no-cache $(BUCK_TARGET))
