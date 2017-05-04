# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../../..

ANALYZER = checkers
CLANG_OPTIONS = -c
INFER_OPTIONS = --biabduction --project-root $(TESTS_DIR)

INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.c)

include $(TESTS_DIR)/clang.make
