# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

ANALYZER = linters
CLANG_OPTIONS = -c -fobjc-arc $(OBJCPP_CLANG_OPTIONS)
INFER_OPTIONS = --no-filtering --debug-exceptions --project-root $(TESTS_DIR) \
	--enable-issue-type GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL
INFERPRINT_OPTIONS = --issues-tests

SOURCES = \
  $(wildcard *.mm) \
  $(wildcard */*.mm) \

include $(TESTS_DIR)/clang.make
include $(TESTS_DIR)/objc.make
