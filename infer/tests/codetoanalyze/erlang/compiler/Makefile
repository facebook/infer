# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

SOURCES = ../pulse/features ../pulse/nonmatch

include $(TESTS_DIR)/base.make

default: test

$(TESTS_DIR)/$(TEST_REL_DIR)/issues.exp.test: $(SOURCES)
	./run.escript $^ > $@

.PHONY: test
test: $(TESTS_DIR)/$(TEST_REL_DIR)/issues.exp.test
	$(QUIET)cd $(TESTS_DIR) && \
	$(call check_no_diff,$(TEST_REL_DIR)/issues.exp,$(TEST_REL_DIR)/issues.exp.test)

.PHONY: clean
clean:
	rm -f *.beam
	rm -f issues.exp.test

.PHONY: replace
replace: issues.exp.test
	cp $< issues.exp
