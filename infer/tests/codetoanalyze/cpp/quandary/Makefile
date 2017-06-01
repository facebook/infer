# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../../..

ANALYZER = checkers
# see explanations in cpp/errors/Makefile for the custom isystem
CLANG_OPTIONS = -x c++ -std=c++11 -nostdinc++ -isystem$(ROOT_DIR) -isystem$(CLANG_INCLUDES)/c++/v1/ -c
INFER_OPTIONS = --quandary --passthroughs --ml-buckets cpp --no-filtering --debug-exceptions --project-root $(TESTS_DIR) --no-failures-allowed
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.cpp)

include $(TESTS_DIR)/clang.make
