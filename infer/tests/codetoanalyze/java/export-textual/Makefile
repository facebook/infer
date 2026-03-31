# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR=../../..

SOURCES = $(sort $(wildcard *.java))

include $(TESTS_DIR)/java.make

CLEAN_EXTRA = *.class

JAVAC_FLAGS = -g -source 8 -target 8

.PHONY: capture
capture: $(SOURCES) $(INFER_BIN)
	$(QUIET)$(call silent_on_success,Capturing with --store-textual in $(TEST_REL_DIR),\
	  $(REMOVE_DIR) infer-out $(EXPORT_DIR) && \
	  $(INFER_BIN) capture --store-textual --project-root $(TESTS_DIR) \
	    -- $(JAVAC) $(JAVAC_FLAGS) $(SOURCES))

include $(TESTS_DIR)/export-textual.make
