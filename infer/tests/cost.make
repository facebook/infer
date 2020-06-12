# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# We add issues.exp.test to recipe in order to avoid "infer report"s run parallel
cost-issues.exp.test$(TEST_SUFFIX): $(INFER_OUT)/report.json $(INFER_BIN) issues.exp.test$(TEST_SUFFIX)
	$(QUIET)$(INFER_BIN) report -q --results-dir $(<D) \
	   $(INFERPRINT_COST_OPTIONS) $@

test: cost-test

.PHONY: cost-test
cost-test: cost-issues.exp.test$(TEST_SUFFIX)
	$(QUIET)cd $(TESTS_DIR) && \
	$(call check_no_diff,$(TEST_REL_DIR)/cost-issues.exp,$(TEST_REL_DIR)/cost-issues.exp.test$(TEST_SUFFIX))


replace: cost-replace

.PHONY: cost-replace
cost-replace: cost-issues.exp.test$(TEST_SUFFIX)
	    cp $< cost-issues.exp$(TEST_RESULT_SUFFIX)


clean: cost-clean

.PHONY: cost-clean
cost-clean:
	$(REMOVE) cost-issues.exp.test$(TEST_SUFFIX)
