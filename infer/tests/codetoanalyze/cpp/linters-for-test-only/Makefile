# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../../..

ANALYZER = linters
CLANG_OPTIONS = -std=c++11 -c  
INFER_OPTIONS = --cxx --linters-def-file linters_example.al --project-root $(TESTS_DIR)
INFERPRINT_OPTIONS = --issues-tests

SOURCES = \
  $(wildcard *.cpp)
  
include $(TESTS_DIR)/clang.make
