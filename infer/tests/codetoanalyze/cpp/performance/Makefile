# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

# see explanations in cpp/pulse/Makefile for the custom isystem
CLANG_OPTIONS = -x c++ -std=c++17 -nostdinc++ -isystem$(CLANG_INCLUDES)/c++/v1/ -c
INFER_OPTIONS = --cost-only --no-filtering --debug-exceptions \
  --project-root $(TESTS_DIR) --report-force-relative-path \
  --no-cost-suppress-func-ptr
INFERPRINT_OPTIONS = --issues-tests
INFERPRINT_COST_OPTIONS = --cost-issues-tests

SOURCES = $(wildcard *.cpp)

HEADERS =

include $(TESTS_DIR)/clang.make
include $(TESTS_DIR)/cost.make

infer-out/report.json: $(MAKEFILE_LIST)
