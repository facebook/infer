# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..
ROOT_DIR = $(TESTS_DIR)/../..

ANALYSE = $(ROOT_DIR)/scripts/genrule_run.sh 
BUCK_TARGET = //module2:module2
INFER_OUT = $(shell pwd)/infer-out
CLEAN_EXTRA = buck-out 
SOURCES = $(shell find . -name '*.java')

INFERPRINT_OPTIONS = --issues-tests
INFER_OPTIONS = --debug-exceptions

include $(TESTS_DIR)/infer.make

$(INFER_OUT)/report.json: $(MAKEFILE_LIST) $(SOURCES)
	$(QUIET) $(REMOVE_DIR) buck-out && \
	$(call silent_on_success,Testing genrule capture integration in $(TEST_REL_DIR),\
	  $(ANALYSE) $(BUCK_TARGET) $(INFER_OUT) $(INFER_OPTIONS))

