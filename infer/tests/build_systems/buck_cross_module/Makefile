# Copyright (c) 2017-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..
ROOT_DIR = $(TESTS_DIR)/../..

BUCK_TARGET = //infer/tests/build_systems/buck_cross_module/module2:module2
INFERPRINT_OPTIONS = --project-root $(ROOT_DIR) --issues-tests
CLEAN_EXTRA = $(ROOT_DIR)/buck-out

include $(TESTS_DIR)/java.make
include $(TESTS_DIR)/infer.make

$(OBJECTS): $(JAVA_SOURCE_FILES)
	$(QUIET)cd $(ROOT_DIR) && \
	$(call silent_on_success,Compiling Buck cross module test tests,\
	INFER_BIN=$(INFER_BIN) \
	$(BUCK) build --no-cache $(BUCK_TARGET))

infer-out/report.json: $(JAVA_DEPS) $(JAVA_SOURCE_FILES)
	$(QUIET)cd $(ROOT_DIR) && \
	$(REMOVE_DIR) buck-out && \
	$(call silent_on_success,Testing Buck Java cross module integration,\
	INFER_BIN=$(INFER_BIN) \
	$(INFER_BIN) --results-dir $(CURDIR)/infer-out -- \
	  $(BUCK) build --no-cache $(BUCK_TARGET))
