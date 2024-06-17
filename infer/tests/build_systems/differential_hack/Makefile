# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..
SOURCES = src/DiffExample.current.hack src/DiffExample.previous.hack
COPIED = src/DiffExample.hack
CLEAN_EXTRA = $(COPIED)
include $(TESTS_DIR)/differential.make

$(CURRENT_REPORT):
	$(QUIET)$(COPY) src/DiffExample.current.hack src/DiffExample.hack
	$(QUIET)$(call silent_on_success,Testing Hack Differential: current,\
	$(INFER_BIN) --no-filtering --pulse-only -o $(CURRENT_DIR) --hackc-binary $(HACKC) \
	  -- $(HACKC) compile-infer $(COPIED))

$(PREVIOUS_REPORT):
	$(QUIET)$(COPY) src/DiffExample.previous.hack src/DiffExample.hack
	$(QUIET)$(call silent_on_success,Testing Hack Differential: previous,\
	$(INFER_BIN) --no-filtering --pulse-only -o $(PREVIOUS_DIR) --hackc-binary $(HACKC) \
	  -- $(HACKC) compile-infer $(COPIED))
