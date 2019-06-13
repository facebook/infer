# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

BUCK_TARGET = //module2:module2
INFER_TARGET = $(BUCK_TARGET)_infer
INFERPRINT_OPTIONS = --issues-tests
SOURCES = $(shell find . -name '*.java')
CLEAN_EXTRA = buck-out
INFER_OUT = buck-out/gen/module2/module2_infer/infer_out
JSON_REPORT = $(INFER_OUT)/report.json

include $(TESTS_DIR)/java.make
include $(TESTS_DIR)/infer.make

$(JSON_REPORT): $(JAVA_DEPS) $(SOURCES) $(MAKEFILE_LIST)
	$(QUIET)$(REMOVE_DIR) buck-out && \
	$(call silent_on_success,Testing Buck genrule for Java integration,\
	INFER_BIN="$(INFER_BIN)" $(BUCK) build --no-cache $(INFER_TARGET))
	$(QUIET)touch $@

issues.exp.test$(TEST_SUFFIX): $(JSON_REPORT) $(INFER_BIN)
	$(QUIET)$(INFER_BIN) report -q \
	   $(INFERPRINT_OPTIONS) $@ --from-json-report $(JSON_REPORT)
