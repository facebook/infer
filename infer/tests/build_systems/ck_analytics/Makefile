# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

CODETOANALYZE_DIR = ../codetoanalyze/componentkit

CLANG_OPTIONS = -x objective-c++ -std=c++11 -c -fblocks
INFER_OPTIONS = --no-capture --linters-only --no-filtering --compute-analytics --project-root $(CODETOANALYZE_DIR)
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(CODETOANALYZE_DIR)/TestComponentKitAnalytics.mm

include $(TESTS_DIR)/clang.make
