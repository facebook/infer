# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..
INFER_OUT ?= infer-out$(TEST_SUFFIX)

include $(TESTS_DIR)/base.make

INFER_OPTIONS = --parse-doli
SOURCES = $(sort $(wildcard *.doli))

.PHONY: compile
compile:

.PHONY: analyze
analyze:

.PHONY: print
print:

.PHONY: test
test: issues.exp.test$(TEST_SUFFIX)
	$(QUIET)cd $(TESTS_DIR) && \
	$(call check_no_diff,$(TEST_REL_DIR)/issues.exp$(TEST_RESULT_SUFFIX),$(TEST_REL_DIR)/issues.exp.test$(TEST_SUFFIX))

issues.exp.test$(TEST_SUFFIX): $(SOURCES) $(INFER_BIN)
	$(QUIET)$(REMOVE_DIR) $(INFER_OUT)
	@for f in $(SOURCES); do $(INFER_BIN) -o $(INFER_OUT) --parse-doli $${f}; done >& $@

.PHONY: replace
replace: issues.exp.test
	cp $< issues.exp

.PHONY: clean
clean:
	$(REMOVE_DIR) issues.exp.test$(TEST_SUFFIX) $(CLEAN_EXTRA)
ifneq ($(INFER_OUT),.)
	$(REMOVE_DIR) $(INFER_OUT)
endif
