# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

CLANG_OPTIONS = -c $(OBJC_CLANG_OPTIONS)
INFER_OPTIONS = --cost-only --debug-exceptions --project-root $(TESTS_DIR) \
  --report-force-relative-path --xcode-isysroot-suffix $(XCODE_ISYSROOT_SUFFIX) \
  --no-cost-suppress-func-ptr
INFERPRINT_OPTIONS = --issues-tests
INFERPRINT_COST_OPTIONS = --cost-tests-only-autoreleasepool --cost-issues-tests

SOURCES = $(wildcard *.m)
SOURCES_ARC = $(wildcard arc_*.m)
SOURCES_NO_ARC = $(filter-out $(SOURCES_ARC),$(SOURCES))

include $(TESTS_DIR)/clang-arc-no-arc.make
include $(TESTS_DIR)/objc.make
include $(TESTS_DIR)/cost.make

infer-out/report.json: $(MAKEFILE_LIST)
