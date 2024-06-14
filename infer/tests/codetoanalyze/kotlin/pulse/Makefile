# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..
INFER_OPTIONS = \
  --pulse-only --debug-exceptions --no-pulse-force-continue --pulse-report-issues-for-tests \
  --report-suppress-errors SENSITIVE_DATA_FLOW --report-suppress-errors DATA_FLOW_TO_SINK
INFERPRINT_OPTIONS = --issues-tests

JAVA_SOURCES = $(wildcard *.java taint/*.java)
KOTLIN_SOURCES = $(wildcard *.kt taint/*.kt)

CLEAN_EXTRA = *.class META-INF

include $(TESTS_DIR)/kotlinc_with_java.make
