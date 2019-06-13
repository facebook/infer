# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

CLANG_OPTIONS = -c $(OBJC_CLANG_OPTIONS)

INFER_OPTIONS = --quandary-only --no-filtering --debug-exceptions --project-root $(TESTS_DIR) --iphoneos-target-sdk-version 8.0
INFERPRINT_OPTIONS = --issues-tests

SOURCES = \
  $(wildcard *.m) \

include $(TESTS_DIR)/clang.make
include $(TESTS_DIR)/objc.make
