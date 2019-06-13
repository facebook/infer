# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


# you need to define the following when including this file:
# - TESTS_DIR, specified before including the file, pointing to infer/tests/
# - INFER_OUT
# - a rule to build $(INFER_OUT)/differential/introduced.json

include $(TESTS_DIR)/base.make

default: analyze

.PHONY: analyze
analyze: $(INFER_OUT)/differential/introduced.json

introduced.exp.test: $(INFER_OUT)/differential/introduced.json $(INFER_BIN)
	$(QUIET)$(INFER_BIN) report -o $(INFER_OUT) \
		--from-json-report $(INFER_OUT)/differential/introduced.json \
		--issues-tests introduced.exp.test
	$(QUIET)$(INFER_BIN) report -o $(INFER_OUT) \
		--from-json-report $(INFER_OUT)/differential/fixed.json \
		--issues-tests fixed.exp.test
	$(QUIET)$(INFER_BIN) report -o $(INFER_OUT) \
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
replace: introduced.exp.test
	$(COPY) introduced.exp.test introduced.exp
	$(COPY) fixed.exp.test fixed.exp
	$(COPY) preexisting.exp.test preexisting.exp

.PHONY: clean
clean:
	$(REMOVE_DIR) *.exp.test $(INFER_OUT) $(CURRENT_DIR) $(PREVIOUS_DIR) \
		$(CLEAN_EXTRA)
