# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..
SWIFT_OPTIONS = -whole-module-optimization
INFER_OPTIONS = --annotation-reachability-only --debug-exceptions --project-root $(TESTS_DIR)
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(realpath $(wildcard *.swift) $(wildcard */*.swift))

include $(TESTS_DIR)/infer.make
include $(TESTS_DIR)/swift-base.make

infer-out/report.json: $(SWIFT_DEPS) $(SOURCES) $(MAKEFILE_LIST)
	$(QUIET)$(call silent_on_success,Testing infer/swift in $(TEST_REL_DIR),\
	  $(INFER_BIN) --results-dir $(@D)  \
	    $(INFER_OPTIONS) -- \
	    $(SWIFTC) $(SWIFT_OPTIONS) $(SOURCES))
