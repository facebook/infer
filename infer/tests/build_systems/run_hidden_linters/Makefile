# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

# this test checks that hidden linters do not run without --no-filtering,
# regardless of what other flags are used to enable them (e.g. --compute-analytics)

TESTS_DIR = ../..

ANALYZER = linters

CODETOANALYZE_DIR = ../codetoanalyze/componentkit

CLANG_OPTIONS = -x objective-c++ -std=c++11 -c -fblocks
INFER_OPTIONS = --compute-analytics --project-root $(CODETOANALYZE_DIR)
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(CODETOANALYZE_DIR)/TestComponentKitAnalytics.mm

include $(TESTS_DIR)/clang.make
