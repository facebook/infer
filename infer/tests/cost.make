# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

cost-issues.exp.test$(TEST_SUFFIX): $(INFER_OUT)/report.json $(INFER_BIN)
	$(QUIET)$(INFER_BIN) report -q --results-dir $(<D) \
	   $(INFERPRINT_COST_OPTIONS) $@

test: cost-test

cost-test: cost-issues.exp.test$(TEST_SUFFIX)
	$(QUIET)cd $(TESTS_DIR) && \
	$(call check_no_diff,$(TEST_REL_DIR)/cost-issues.exp,$(TEST_REL_DIR)/cost-issues.exp.test$(TEST_SUFFIX))


replace: cost-replace

cost-replace: cost-issues.exp.test$(TEST_SUFFIX)
	    cp $< cost-issues.exp$(TEST_RESULT_SUFFIX)


clean: cost-clean

cost-clean:
	rm cost-issues.exp.test$(TEST_SUFFIX)
