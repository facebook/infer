# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

ANALYZER = checkers

CLANG_OPTIONS = -c
INFER_OPTIONS = --project-root src --liveness
INFERPRINT_OPTIONS = --issues-tests

SOURCES = src/templates.cpp

include $(TESTS_DIR)/clang.make
