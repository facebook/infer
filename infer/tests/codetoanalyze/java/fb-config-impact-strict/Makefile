# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

INFER_OPTIONS = --config-impact-analysis-only --config-impact-strict-mode \
  --debug-exceptions --report-force-relative-path \
  --config-impact-config-field-patterns "ConfigValues2\\.enable_feature" \
  --config-impact-config-function-patterns "ConfigValues\\.enable_feature" \
  --config-impact-config-param-patterns "Basic\\.gated_by_known_config_param_ok enable_feature"
INFERPRINT_OPTIONS = --issues-tests
SOURCES = $(wildcard *.java)

include $(TESTS_DIR)/javac.make
include $(TESTS_DIR)/config-impact.make
