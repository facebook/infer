# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

INFER_OUT ?= infer-out$(TEST_SUFFIX)

include $(TESTS_DIR)/base.make

default: test

issues.exp.test$(TEST_SUFFIX): $(INFER_OUT)/report.json $(INFER_BIN) $(MAKEFILE_LIST)
	$(QUIET)$(INFER_BIN) report -q --results-dir $(<D) \
	   $(INFERPRINT_OPTIONS) $@

.PHONY: analyze
analyze: $(INFER_OUT)/report.json

.PHONY: print
print: issues.exp.test$(TEST_SUFFIX)

.PHONY: test
test: issues.exp.test$(TEST_SUFFIX)
	$(QUIET)cd $(TESTS_DIR) && \
	$(call check_no_diff,$(TEST_REL_DIR)/issues.exp$(TEST_RESULT_SUFFIX),$(TEST_REL_DIR)/issues.exp.test$(TEST_SUFFIX))
	$(QUIET)$(call check_no_duplicates,$(INFER_OUT)/duplicates.txt)

.PHONY: replace
replace: issues.exp.test$(TEST_SUFFIX)
	cp $< issues.exp$(TEST_RESULT_SUFFIX)

.PHONY: clean
clean:
	$(REMOVE_DIR) codetoanalyze com issues.exp.test$(TEST_SUFFIX) $(CLEAN_EXTRA)
ifneq ($(INFER_OUT),.)
	$(REMOVE_DIR) $(INFER_OUT)
endif
