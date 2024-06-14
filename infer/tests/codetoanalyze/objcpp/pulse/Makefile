# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

CLANG_OPTIONS = -c $(OBJCPP_CLANG_OPTIONS) -fobjc-arc
INFER_OPTIONS = \
  --pulse-only --debug-exceptions --no-pulse-force-continue --project-root $(TESTS_DIR) \
  --pulse-report-issues-for-tests \
  --pulse-model-alloc-pattern "AB[IF].*Create.*" \
  --pulse-model-release-pattern "ABFRelease" \
  --pulse-model-transfer-ownership "abf::BridgingRelease" \
  --pulse-model-transfer-ownership "ABFBridgingRelease" \
  --pulse-taint-config taint_config_objcpp.json

INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.mm)

include $(TESTS_DIR)/clang.make
include $(TESTS_DIR)/objc.make

infer-out/report.json: $(MAKEFILE_LIST)
