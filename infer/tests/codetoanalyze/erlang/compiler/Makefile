# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

SOURCES = ../pulse/features ../pulse/nonmatch ../pulse/specs ../pulse-otp ../topl/less ../topl/fields ../topl/taint ../topl/taint-genserver ../topl/process ../topl/atom-literal ../topl/str-literal

include $(TESTS_DIR)/base.make

default: test

issues.exp.test: $(SOURCES)
	./run.escript $^ > $@

.PHONY: test
test: issues.exp.test
	$(QUIET)cd $(TESTS_DIR) && \
	$(call check_no_diff,$(TEST_REL_DIR)/issues.exp,$(TEST_REL_DIR)/issues.exp.test)

.PHONY: clean
clean:
	rm -f *.beam
	rm -f issues.exp.test

.PHONY: replace
replace: issues.exp.test
	cp $< issues.exp
