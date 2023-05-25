# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..
include $(TESTS_DIR)/base.make
SOURCES = $(sort $(wildcard *.sil))  $(sort $(wildcard *.doli)) $(sort $(wildcard *.java))
INFER_SETTINGS = -j 1 --no-progress-bar --pulse-only -o infer-out

default: test

test: issues.exp.test
issues.exp.test:
	$(INFER_BIN) $(INFER_SETTINGS)  --capture-doli DoliDecls.doli -- javac CheckDoliImports.java
	$(INFER_BIN) report -o infer-out --issues-tests issues.exp.test


.PHONY: test
test: issues.exp.test
	$(QUIET)cd $(TESTS_DIR) && \
	 $(call check_no_diff,$(TEST_REL_DIR)/issues.exp$(TEST_RESULT_SUFFIX),$(TEST_REL_DIR)/issues.exp.test$(TEST_SUFFIX))

.PHONY: replace
replace: issues.exp.test
	cp $< issues.exp

clean:
	$(REMOVE) issues.exp.test
	$(REMOVE_DIR) infer-out codetoanalyze
