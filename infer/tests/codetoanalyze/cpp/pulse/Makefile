# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

# use our own clang's standard library so that the tests are uniform across distributions
CLANG_OPTIONS = -x c++ -std=c++17 -nostdinc++ -isystem$(CLANG_INCLUDES)/c++/v1/ -c
INFER_OPTIONS = \
  --pulse-only --debug-exceptions --project-root $(TESTS_DIR) \
  --pulse-report-issues-for-tests --no-pulse-force-continue \
  --report-suppress-errors SENSITIVE_DATA_FLOW --report-suppress-errors DATA_FLOW_TO_SINK
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.cpp taint/*.cpp)

include $(TESTS_DIR)/clang.make

infer-out/report.json: $(MAKEFILE_LIST)
