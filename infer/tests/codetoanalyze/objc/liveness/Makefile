# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

ANALYZER = checkers
CLANG_OPTIONS = -x objective-c++ -std=c++11 -fobjc-arc -c
INFER_OPTIONS = --liveness-only --debug-exceptions --project-root $(TESTS_DIR)
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.m)

include $(TESTS_DIR)/clang.make

infer-out/report.json: $(MAKEFILE_LIST)
