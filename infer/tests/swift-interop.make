# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

include $(TESTS_DIR)/base.make
include $(TESTS_DIR)/objc.make
include $(TESTS_DIR)/swift-base.make

# --- Generic Sources ---
OBJC_SOURCES = $(wildcard *.m)
SWIFT_SOURCES = $(wildcard *.swift)
BRIDGING_HEADER = BridgingHeader.h

# --- Output & Expectations ---
INFER_OUT ?= infer-out
BITCODE_FILE = swift_logic.bc
EXPECTED_ISSUES = issues.exp

# --- Shared Flags & Defaults ---
SWIFT_OPTIONS = -import-objc-header $(abspath $(BRIDGING_HEADER)) -I $(abspath .)
INFERPRINT_OPTIONS = --issues-tests

# --- Recipes ---
default: test

$(INFER_OUT)/report.json: $(OBJC_SOURCES) $(SWIFT_SOURCES) $(BRIDGING_HEADER)
	$(QUIET)$(REMOVE_DIR) $(INFER_OUT)
	$(QUIET)$(call silent_on_success,Capture Objective-C, \
	  $(INFER_BIN) capture --results-dir $(INFER_OUT) -- clang -c $(OBJC_SOURCES) $(OBJC_CLANG_OPTIONS))
	$(QUIET)$(call silent_on_success,Generate Swift Bitcode, \
	  $(SWIFTC) $(SWIFT_OPTIONS) $(foreach src,$(SWIFT_SOURCES),$(abspath $(src))) -emit-bc -o $(BITCODE_FILE))
	$(QUIET)$(call silent_on_success,Ingest Swift Bitcode, \
	  $(INFER_BIN) capture --continue --results-dir $(INFER_OUT) \
	  --llvm-bitcode-file $(abspath $(BITCODE_FILE)) \
	  $(foreach src,$(SWIFT_SOURCES),--llvm-bitcode-source $(abspath $(src))))
	$(QUIET)$(call silent_on_success,Analyze Interop, \
	  $(INFER_BIN) analyze --results-dir $(INFER_OUT) $(INFER_OPTIONS))

issues.exp.test: $(INFER_OUT)/report.json
	$(QUIET)$(INFER_BIN) report -q --results-dir $(INFER_OUT) \
	   $(INFERPRINT_OPTIONS) $@

.PHONY: test
test: issues.exp.test
	$(QUIET)$(call check_no_diff,$(EXPECTED_ISSUES),issues.exp.test)

.PHONY: replace
replace: issues.exp.test
	cp issues.exp.test $(EXPECTED_ISSUES)

.PHONY: clean
clean:
	$(QUIET)$(REMOVE_DIR) $(INFER_OUT)
	$(QUIET)$(REMOVE) *.bc *.o.tmp issues.exp.test
