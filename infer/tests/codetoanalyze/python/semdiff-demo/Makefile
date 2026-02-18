# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

include $(TESTS_DIR)/base.make

DEMO_DIR = $(CURDIR)
INFER_BIN ?= $(BIN_DIR)/infer

CONFIGS = $(sort $(wildcard configs/*.py))
EXAMPLES = $(sort $(wildcard examples/*/before.py) $(wildcard examples/*/after.py))

default: test

demo.exp.test: run_demo.sh $(CONFIGS) $(EXAMPLES) $(INFER_BIN)
	$(QUIET)INFER=$(INFER_BIN) bash run_demo.sh > $@

.PHONY: test
test: demo.exp.test
	$(QUIET)cd $(TESTS_DIR) && \
	$(call check_no_diff,$(TEST_REL_DIR)/demo.exp,$(TEST_REL_DIR)/demo.exp.test)

.PHONY: replace
replace: demo.exp.test
	cp $< demo.exp

.PHONY: clean
clean:
	$(REMOVE_DIR) demo.exp.test
