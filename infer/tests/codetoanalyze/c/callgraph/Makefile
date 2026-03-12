# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..
ROOT_DIR = $(TESTS_DIR)/../..

include $(ROOT_DIR)/Makefile.config
include $(TESTS_DIR)/base.make

SOURCES = $(wildcard *.c)

default: test

infer-out/specialized_call_graph.json: $(SOURCES) $(MAKEFILE_LIST)
	$(QUIET)$(INFER_BIN) -q -- clang -c $(SOURCES)
	$(QUIET)$(INFER_BIN) debug --procedures --dump-json-specialized-call-graph

callgraph.exp.test: infer-out/specialized_call_graph.json parse_json_callgraph.py
	$(QUIET)python3 parse_json_callgraph.py $< > $@

ROOTS = f \
        test_aliased test_not_aliased \
        box_test_aliased box_test_not_aliased \
        test_add_0 test_add_1 test_mul_0 test_mul_1 \
        box2_test_add_0 box2_test_add_1 box2_test_mul_0 box2_test_mul_1

callchains.exp.test: infer-out/specialized_call_graph.json parse_json_callgraph.py
	$(QUIET)python3 parse_json_callgraph.py $< paths $(ROOTS) > $@

.PHONY: test
test: callgraph.exp.test callchains.exp.test
	$(QUIET)cd $(TESTS_DIR) && \
	$(call check_no_diff,$(TEST_REL_DIR)/callgraph.exp,$(TEST_REL_DIR)/callgraph.exp.test)
	$(QUIET)cd $(TESTS_DIR) && \
	$(call check_no_diff,$(TEST_REL_DIR)/callchains.exp,$(TEST_REL_DIR)/callchains.exp.test)

.PHONY: replace
replace: callgraph.exp.test callchains.exp.test
	cp callgraph.exp.test callgraph.exp
	cp callchains.exp.test callchains.exp

.PHONY: clean
clean:
	$(REMOVE_DIR) infer-out callgraph.exp.test callchains.exp.test *.o
