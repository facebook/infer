# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

CLANG_OPTIONS = -c $(OBJCPP_CLANG_OPTIONS) -fobjc-arc
INFER_OPTIONS =  --debug-exceptions --project-root $(TESTS_DIR)
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.mm)

include $(TESTS_DIR)/clang.make
include $(TESTS_DIR)/objc.make

infer-out/report.json: $(MAKEFILE_LIST)
