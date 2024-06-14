# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

CLANG_OPTIONS_C = -c
CLANG_OPTIONS_CPP = -x c++ -std=c++17 -nostdinc++ -isystem$(CLANG_INCLUDES)/c++/v1/ -c

INFER_OPTIONS = --pulse-only --debug-exceptions --no-pulse-force-continue --project-root $(TESTS_DIR)

INFERPRINT_OPTIONS = --issues-tests

SOURCES_C = $(wildcard *.c)
SOURCES_CPP = $(wildcard *.cpp)

include $(TESTS_DIR)/clang-c-cpp.make
