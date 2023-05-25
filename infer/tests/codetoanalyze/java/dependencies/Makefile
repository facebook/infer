# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..
INFER_OPTIONS = --dependencies
INFERPRINT_OPTIONS = --issues-tests-fields "file,procedure,line_offset,bug_type,bucket,severity,bug_trace" --issues-tests
SOURCES = my/Application.java
CLEAN_EXTRA += $(wildcard my/*.class) $(wildcard lib/*.class)

include $(TESTS_DIR)/infer.make

infer-out/report.json: $(JAVA_DEPS) $(MAKEFILE_LIST) $(SOURCES) .inferconfig
	$(QUIET)$(call silent_on_success,Testing infer with dependencies mode in $(TEST_REL_DIR),\
	  $(INFER_BIN) --results-dir $(@D) $(INFER_OPTIONS) -- $(JAVAC) $(SOURCES))
