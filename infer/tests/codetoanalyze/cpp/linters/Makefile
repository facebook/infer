# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

CLANG_OPTIONS = -std=c++11 -c
INFER_OPTIONS = --no-capture --linters-only --linters-def-file extracopy.al --linters-def-file call_function.al --linters-def-file call_cxx_method.al --no-filtering --debug-exceptions --project-root $(TESTS_DIR) \
--enable-issue-type GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.cpp)

include $(TESTS_DIR)/clang.make
