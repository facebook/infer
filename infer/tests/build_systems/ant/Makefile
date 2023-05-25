# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

SOURCES = $(wildcard src/*.java)
INFERPRINT_OPTIONS = --issues-tests
CLEAN_EXTRA = ant_out

include $(TESTS_DIR)/java.make
include $(TESTS_DIR)/infer.make

infer-out/report.json: $(JAVA_DEPS) $(SOURCES)
	$(QUIET)$(call silent_on_success,Testing ant integration,\
	  $(ANT) clean && $(INFER_BIN) --project-root $(TESTS_DIR) -- $(ANT))
