# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

CLANG_OPTIONS = -fobjc-arc -c $(OBJCPP_CLANG_OPTIONS)
INFER_OPTIONS = --no-capture --linters-only --cxx --linters-def-file linters_example.al --project-root $(TESTS_DIR)
INFERPRINT_OPTIONS = --issues-tests

SOURCES = \
  $(wildcard *.m) \
  $(wildcard */*.m) \
  $(wildcard *.mm) \
  $(wildcard *.c) \

include $(TESTS_DIR)/clang.make
include $(TESTS_DIR)/objc.make
