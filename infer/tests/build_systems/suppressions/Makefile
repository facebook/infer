# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

SOURCES = $(wildcard src/*.cpp)
CLANG_OPTIONS = -c
INFERPRINT_OPTIONS = --issues-tests-fields "bug_type,file,procedure,qualifier,suppressed" --issues-tests

include $(TESTS_DIR)/clang.make
