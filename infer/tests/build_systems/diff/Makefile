# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..

include $(TESTS_DIR)/base.make

SRC_DIR = $(CURDIR)/../codetoanalyze
INFER_OUT = infer-out
INFER_OPTIONS = \
  --previous-to-current-script '$(COPY) $(SRC_DIR)/some_bugs.c src/hello.c' \
  --current-to-previous-script '$(COPY) $(SRC_DIR)/some_different_bugs.c src/hello.c' \
  --report-hook '/bin/true' \
  -- clang -c src/hello.c

SOURCES = $(SRC_DIR)/some_bugs.c $(SRC_DIR)/some_different_bugs.c

default: analyze

.PHONY: analyze
analyze: $(INFER_OUT)/differential/introduced.json

$(INFER_OUT)/differential/introduced.json: $(SOURCES) $(CLANG_DEPS)
	$(QUIET)$(MKDIR_P) src
	$(QUIET)$(COPY) $(SRC_DIR)/some_bugs.c src/hello.c
	$(QUIET)$(call silent_on_success,Running diff analysis in $(TEST_REL_DIR),\
	  $(INFER_BIN) -o $(INFER_OUT) --project-root $(CURDIR) diff \
	        $(INFER_OPTIONS))

introduced.exp.test: $(INFER_OUT)/differential/introduced.json $(INFER_BIN)
	$(QUIET)$(INFER_BIN) report \
		--from-json-report $(INFER_OUT)/differential/introduced.json \
		--issues-tests introduced.exp.test
	$(QUIET)$(INFER_BIN) report \
		--from-json-report $(INFER_OUT)/differential/fixed.json \
		--issues-tests fixed.exp.test
	$(QUIET)$(INFER_BIN) report \
		--from-json-report $(INFER_OUT)/differential/preexisting.json \
		--issues-tests preexisting.exp.test

.PHONY: print
print: introduced.exp.test

.PHONY: test
test: print
	$(QUIET)$(call check_no_diff,introduced.exp,introduced.exp.test)
	$(QUIET)$(call check_no_diff,fixed.exp,fixed.exp.test)
	$(QUIET)$(call check_no_diff,preexisting.exp,preexisting.exp.test)

.PHONY: replace
replace: $(EXPECTED_TEST_OUTPUT)
	$(COPY) introduced.exp.test introduced.exp
	$(COPY) fixed.exp.test fixed.exp
	$(COPY) preexisting.exp.test preexisting.exp

.PHONY: clean
clean:
	$(REMOVE_DIR) *.exp.test $(INFER_OUT) $(CURRENT_DIR) $(PREVIOUS_DIR) \
		$(CLEAN_EXTRA)
