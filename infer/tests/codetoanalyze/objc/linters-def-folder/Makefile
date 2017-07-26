# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../../..

ANALYZER = linters
CLANG_OPTIONS = -c
INFER_OPTIONS = --linters-def-folder checks --project-root $(TESTS_DIR) --no-keep-going
INFERPRINT_OPTIONS = --issues-tests

SOURCES = file.m

include $(TESTS_DIR)/clang.make
