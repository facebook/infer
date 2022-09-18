# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..
INFER_OUT ?= infer-out$(TEST_SUFFIX)

include $(TESTS_DIR)/base.make

SOURCES = $(sort $(wildcard *.sil))
FILE_REPORTS = $(patsubst %.sil,%.exp.test,$(SOURCES))

# infer-outs from --capture-textual
CAPTURE_DIR = capture-outs

CLEAN_EXTRA = $(CAPTURE_DIR) *.exp.test

default: test

%.exp.test: %.sil $(INFER_BIN)
	$(INFER_BIN) --quiet --no-progress-bar --pulse-only --debug-exceptions \
		-o "$(CAPTURE_DIR)/infer-out-$(basename $<)" --capture-textual $<
	$(INFER_BIN) report -q --results-dir "$(CAPTURE_DIR)/infer-out-$(basename $<)" --issues-tests $@

.PHONY: compile
compile: $(FILE_REPORTS)

issues.exp.test: $(FILE_REPORTS)
	cat $^ > $@

.PHONY: test
test: issues.exp.test
	$(QUIET)cd $(TESTS_DIR) && \
	$(call check_no_diff,$(TEST_REL_DIR)/issues.exp$(TEST_RESULT_SUFFIX),$(TEST_REL_DIR)/issues.exp.test$(TEST_SUFFIX))

.PHONY: replace
replace: issues.exp.test
	cp $< issues.exp

.PHONY: clean
clean:
	$(REMOVE_DIR) issues.exp.test$(TEST_SUFFIX) $(OBJECTS) $(CLEAN_EXTRA)
ifneq ($(INFER_OUT),.)
	$(REMOVE_DIR) $(INFER_OUT)
endif
