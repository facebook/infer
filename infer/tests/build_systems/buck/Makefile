# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

BUCK_TARGET = //module2:module2
INFER_OPTIONS = --eradicate --no-linters
INFERPRINT_OPTIONS = --issues-tests
CLEAN_EXTRA = buck-out
SOURCES = $(shell find . -name '*.java')


include $(TESTS_DIR)/java.make
include $(TESTS_DIR)/infer.make

infer-out/report.json: $(JAVA_DEPS) $(SOURCES)
	$(QUIET)$(REMOVE_DIR) buck-out && \
	$(call silent_on_success,Testing Buck Java integration,\
	INFER_BIN=$(INFER_BIN) \
	  $(INFER_BIN) $(INFER_OPTIONS) --results-dir $(CURDIR)/infer-out -- \
	  $(BUCK) build --deep --no-cache $(BUCK_TARGET))
