# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..
ROOT_DIR = $(TESTS_DIR)/../..

BUCK_TARGET = //module2:module2
CLEAN_EXTRA = buck-out
SOURCES = $(shell find . -name '*.java')

INFERPRINT_OPTIONS = --issues-tests
INFER_OPTIONS = --buck-java --debug-exceptions

include $(TESTS_DIR)/infer.make

$(INFER_OUT)/report.json: $(MAKEFILE_LIST) $(SOURCES)
	$(QUIET)$(BUCK) clean
	$(call silent_on_success,Testing java integration in $(TEST_REL_DIR),\
	  $(INFER_BIN) --results-dir $(@D) $(INFER_OPTIONS) -- $(BUCK) build --no-cache $(BUCK_TARGET))
