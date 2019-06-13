# Copyright (c) 2016-present, Programming Research Laboratory (ROPAS)
#                             Seoul National University, Korea
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

CLANG_OPTIONS = -c
INFER_OPTIONS = -F --project-root $(TESTS_DIR) --bufferoverrun-only --function-pointer-specialization
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.c)

include $(TESTS_DIR)/clang.make
