# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..
ROOT_DIR = $(TESTS_DIR)/../..

include $(TESTS_DIR)/base.make

toplevel.exp.test: $(BUILD_DIR)/test/infertop.bc $(SCRIPT_DIR)/infer_repl \
                   $(SCRIPT_DIR)/toplevel_init $(INFER_DIR)/tests/repl/infer_batch_script.mltop
	$(QUIET)$(call silent_on_success,Testing infer OCaml REPL,\
	BUILD_DIR=$(BUILD_DIR)/test \
	$(SCRIPT_DIR)/infer_repl $(INFER_DIR)/tests/repl/infer_batch_script.mltop | \
	sed -e 's#^The files .*/extLib.cma$$#The files [...]/extLib.cma#' \
	    -e 's#^and .*/infertop.bc$$#and [...]/infertop.bc#'	\
	 > $@)

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
