# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

INFER_OPTIONS = --config-impact-analysis-only --config-impact-data-file config_data.json \
  --debug-exceptions --report-force-relative-path
INFERPRINT_OPTIONS = --issues-tests
SOURCES = $(wildcard *.java)

include $(TESTS_DIR)/javac.make
include $(TESTS_DIR)/config-impact.make
