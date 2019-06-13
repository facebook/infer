# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

CLANG_OPTIONS = -c -fobjc-arc $(OBJC_CLANG_OPTIONS)
INFER_OPTIONS = --no-capture --linters-only --linters-def-folder checks --project-root $(TESTS_DIR)
INFERPRINT_OPTIONS = --issues-tests

SOURCES = file.m

include $(TESTS_DIR)/clang.make
include $(TESTS_DIR)/objc.make
