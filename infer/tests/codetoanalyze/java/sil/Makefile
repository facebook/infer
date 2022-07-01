# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

include $(TESTS_DIR)/base.make

SOURCES = $(sort $(wildcard *.java))
CLASS_FILES = $(patsubst %.java,%.class,$(SOURCES))
SIL_FILES = $(patsubst %.java,%.sil,$(SOURCES))

.PHONY: compile
compile:

.PHONY: analyze
analyze:

.PHONY: print
print:

.PHONY: test
test: issues.exp.test
	$(QUIET)cd $(TESTS_DIR) && \
	$(call check_no_diff,$(TEST_REL_DIR)/issues.exp$(TEST_RESULT_SUFFIX),$(TEST_REL_DIR)/issues.exp.test$(TEST_SUFFIX))

issues.exp.test: $(SIL_FILES) $(INFER_BIN)
	@for f in $(SIL_FILES); do $(INFER_BIN) --capture-textual-sil $${f}; done >& $@

.PHONY: replace
replace: issues.exp.test
	cp $< issues.exp

.PHONY: clean
clean:
	$(REMOVE_DIR) issues.exp.test$(TEST_SUFFIX) $(CLASS_FILES) $(CLEAN_EXTRA)
ifneq ($(INFER_OUT),.)
	$(REMOVE_DIR) $(INFER_OUT)
endif
