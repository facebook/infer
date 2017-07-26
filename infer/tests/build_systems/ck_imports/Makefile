# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..

ANALYZER = linters

CODETOANALYZE_DIR = ../codetoanalyze/componentkit

CLANG_OPTIONS = -x objective-c++ -std=c++11 -c -fblocks
INFER_OPTIONS = --no-filtering --project-root $(CODETOANALYZE_DIR) --no-keep-going
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(CODETOANALYZE_DIR)/TestIgnoreImports.mm

include $(TESTS_DIR)/clang.make
