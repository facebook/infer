# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# We add issues.exp.test to recipe in order to avoid "infer report"s run parallel
config-impact-issues.exp.test: $(INFER_OUT)/report.json $(INFER_BIN) issues.exp.test
	$(QUIET)$(INFER_BIN) report -q --results-dir $(<D) --config-impact-issues-tests $@

test: config-impact-test

.PHONY: config-impact-test
config-impact-test: config-impact-issues.exp.test
	$(QUIET)cd $(TESTS_DIR) && \
	$(call check_no_diff,$(TEST_REL_DIR)/config-impact-issues.exp,$(TEST_REL_DIR)/config-impact-issues.exp.test)

replace: config-impact-replace

.PHONY: config-impact-replace
config-impact-replace: config-impact-issues.exp.test
	cp $< config-impact-issues.exp

clean: config-impact-clean

.PHONY: config-impact-clean
config-impact-clean:
	$(REMOVE) config-impact-issues.exp.test
