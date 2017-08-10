# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..
ROOT_DIR = $(TESTS_DIR)/../..

include $(TESTS_DIR)/base.make

toplevel.exp.test: $(BUILD_DIR)/test/infertop.bc
	$(QUIET)$(call silent_on_success,Testing infer OCaml REPL,\
	TOPLEVEL_DIR=$(BUILD_DIR)/test $(SCRIPT_DIR)/infer_repl \
	  $(INFER_DIR)/tests/repl/infer_batch_script.mltop > $@)

.PHONY: print
print: toplevel.exp.test

.PHONY: test
test: toplevel.exp.test
	$(QUIET)diff -u toplevel.exp $<

.PHONY: replace
replace: toplevel.exp.test
	$(QUIET)$(COPY) $< toplevel.exp

.PHONY: clean
clean:
	$(QUIET)$(REMOVE) toplevel.exp.test
