# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..
INFER_OUT ?= infer-out$(TEST_SUFFIX)

include $(TESTS_DIR)/base.make

SOURCES = $(sort $(wildcard *.doli))

# infer-out from --capture-textual
CAPTURE_DIR = infer-out-capture
CAPTURE_CMD = $(patsubst %.doli, --capture-doli  %.doli, $(SOURCES))

CLEAN_EXTRA = $(CAPTURE_DIR)

issues.exp.test$(TEST_SUFFIX): $(SOURCES) $(INFER_BIN)
	$(QUIET)$(REMOVE_DIR) $(CAPTURE_DIR)
	$(QUIET)$(INFER_BIN) -j 1 --quiet --no-progress-bar -o $(CAPTURE_DIR) $(CAPTURE_CMD) &> $@

.PHONY: compile
compile: issues.exp.test$(TEST_SUFFIX)

.PHONY: test
test: issues.exp.test$(TEST_SUFFIX)
	$(QUIET)cd $(TESTS_DIR) && \
	$(call check_no_diff,$(TEST_REL_DIR)/issues.exp$(TEST_RESULT_SUFFIX),$(TEST_REL_DIR)/issues.exp.test$(TEST_SUFFIX))

.PHONY: replace
replace: issues.exp.test$(TEST_SUFFIX)
	cp $< issues.exp$(TEST_RESULT_SUFFIX)

.PHONY: clean
clean:
	$(REMOVE_DIR) issues.exp.test$(TEST_SUFFIX) $(CLEAN_EXTRA)
ifneq ($(INFER_OUT),.)
	$(REMOVE_DIR) $(INFER_OUT)
endif
