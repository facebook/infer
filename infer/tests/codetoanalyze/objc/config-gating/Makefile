# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

CLANG_OPTIONS = -c $(OBJC_CLANG_OPTIONS) -fobjc-arc
INFER_OPTIONS = --config-gating-only --debug-exceptions \
  --config-gating-method 'Configs\.feature_x' \
  --config-gating-method 'Configs\.feature_y' \
  --config-gating-method 'Configs\.feature_z' \
  --config-gating-method 'Configs\.stored_config' \
  --config-gating-method 'Config\.isEnabled:' \
  --config-gating-method 'Config\.getValue:' \
  --config-gating-method 'Config\.featureEnabled' \
  --config-gating-report-ungated \
  --config-gating-blocklist 'thisShouldBeBlocklisted'
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.m)

include $(TESTS_DIR)/clang.make
include $(TESTS_DIR)/objc.make

infer-out/report.json: $(MAKEFILE_LIST)
