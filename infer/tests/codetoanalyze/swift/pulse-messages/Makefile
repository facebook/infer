# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..
SWIFT_OPTIONS = -whole-module-optimization
INFER_OPTIONS = \
  --pulse-only --debug-exceptions --project-root $(TESTS_DIR) \
  --pulse-report-issues-for-tests --jobs 1 \


# Test the user-facing message text (qualifier + suggestion) for Pulse Swift
# diagnostics, mirroring what gets shown on the console / in the UI. This is the
# Swift counterpart of build_systems/pulse_messages_objc/.
INFERPRINT_OPTIONS = --issues-tests-fields file,procedure,line,column,severity,bug_type,qualifier,suggestion --issues-tests

SOURCES = $(realpath $(wildcard *.swift))

include $(TESTS_DIR)/infer.make
include $(TESTS_DIR)/swift-base.make

infer-out/report.json: $(SWIFT_DEPS) $(SOURCES) $(MAKEFILE_LIST)
	$(QUIET)$(call silent_on_success,Testing infer/swift in $(TEST_REL_DIR),\
	  $(INFER_BIN) --results-dir $(@D)  \
	    $(INFER_OPTIONS) -- \
	    $(SWIFTC) $(SWIFT_OPTIONS) $(SOURCES))
