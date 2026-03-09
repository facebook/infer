# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..
ROOT_DIR = $(TESTS_DIR)/../..

include $(ROOT_DIR)/Makefile.config
include $(TESTS_DIR)/base.make

SOURCES = $(wildcard *.c)

JQ_QUERY = '.[] | .[0][1].c_name[0] as $$caller | .[1][][1].main.non_disj[] | select(type == "object") | "\(.loc.file[1]):\(.loc.line):\(.loc.col) \($$caller) -> \(.proc_name[1].c_name[0])"'

default: test

infer-out/specialized_call_graph.json: $(SOURCES) $(MAKEFILE_LIST)
	$(QUIET)$(INFER_BIN) -q -- clang -c $(SOURCES)
	$(QUIET)$(INFER_BIN) debug --procedures --dump-json-specialized-call-graph

callgraph.exp.test: infer-out/specialized_call_graph.json
	$(QUIET)jq -r $(JQ_QUERY) $< | sort > $@

.PHONY: test
test: callgraph.exp.test
	$(QUIET)cd $(TESTS_DIR) && \
	$(call check_no_diff,$(TEST_REL_DIR)/callgraph.exp,$(TEST_REL_DIR)/callgraph.exp.test)

.PHONY: replace
replace: callgraph.exp.test
	cp $< callgraph.exp

.PHONY: clean
clean:
	$(REMOVE_DIR) infer-out callgraph.exp.test *.o
