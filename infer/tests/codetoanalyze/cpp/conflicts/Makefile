# Copyright (c) 2017-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


TESTS_DIR = ../../..

ANALYZER = checkers
CLANG_OPTIONS = -c -x c++ -std=c++14
INFER_OPTIONS = --no-default-checkers --biabduction --check-nullable --project-root $(TESTS_DIR)
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.cpp)

include $(TESTS_DIR)/clang.make

infer-out/report.json: $(MAKEFILE_LIST)
