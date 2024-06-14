# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

CLANG_OPTIONS = $(IOS_CLANG_OPTIONS) -x objective-c++ -std=c++17 -nostdinc++ -isystem$(CLANG_INCLUDES)/c++/v1/ -c
INFER_OPTIONS = \
  --pulse-only --debug-exceptions --no-pulse-force-continue --project-root $(TESTS_DIR) \
  --pulse-taint-config .infertaintconfig \
  --pulse-report-issues-for-tests --report-force-relative-path

INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.mm)

include $(TESTS_DIR)/clang.make
include $(TESTS_DIR)/objc.make

infer-out/report.json: $(MAKEFILE_LIST)
