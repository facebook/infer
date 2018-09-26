# Copyright (c) 2018-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

# see explanations in cpp/errors/Makefile for the custom isystem
CLANG_OPTIONS = -x c++ -std=c++14 -Wc++1z-extensions -nostdinc++ -isystem$(MODELS_DIR)/cpp/include -isystem$(CLANG_INCLUDES)/c++/v1/ -c
INFER_OPTIONS = --ownership-only --ml-buckets cpp --debug-exceptions --project-root $(TESTS_DIR)
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.cpp)

include $(TESTS_DIR)/clang.make

infer-out/report.json: $(MAKEFILE_LIST)
