# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

CLANG_OPTIONS = -c $(OBJCPP_CLANG_OPTIONS) -fobjc-arc
INFER_OPTIONS = \
  --pulse-only --debug-exceptions --no-pulse-force-continue --pulse-taint-config taint_config.json \
  --project-root $(TESTS_DIR)

INFERPRINT_OPTIONS = --project-root $(TESTS_DIR) --issues-tests

SOURCES = $(wildcard *.mm) $(wildcard *.m)

include $(TESTS_DIR)/clang.make
include $(TESTS_DIR)/objc.make

infer-out/report.json: $(MAKEFILE_LIST)
