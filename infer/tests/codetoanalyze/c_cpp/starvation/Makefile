# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

ANALYZER = checkers
CLANG_OPTIONS = -c

INFER_OPTIONS = --starvation-only --starvation-whole-program --debug-exceptions --project-root $(TESTS_DIR) --struct-as-cpp-class

INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.c) $(wildcard *.cpp) 

include $(TESTS_DIR)/clang.make

infer-out/report.json: $(MAKEFILE_LIST)
