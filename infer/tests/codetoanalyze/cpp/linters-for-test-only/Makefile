# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

CLANG_OPTIONS = -std=c++11 -c
INFER_OPTIONS = --no-capture --linters-only --cxx --linters-def-file linters_example.al --project-root $(TESTS_DIR) --custom-symbols '{ "test": [ "::AllowedSym" ] }'
INFERPRINT_OPTIONS = --issues-tests

SOURCES = \
  $(wildcard *.cpp)

include $(TESTS_DIR)/clang.make
