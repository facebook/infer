# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..
INFER_OPTIONS = --pulse-only --debug-exceptions --pulse-report-issues-for-tests \
--report-suppress-errors SENSITIVE_DATA_FLOW --report-suppress-errors DATA_FLOW_TO_SINK
INFERPRINT_OPTIONS = --issues-tests
SOURCES = $(wildcard *.java taint/*.java *.kt taint/*.kt)
CLEAN_EXTRA = *.class META-INF

include $(TESTS_DIR)/kotlinc.make
