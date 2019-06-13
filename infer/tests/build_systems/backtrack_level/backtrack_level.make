# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

CLANG_OPTIONS = -c
INFER_OPTIONS = --backtrack-level $(BACKTRACK_LEVEL)
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(TESTS_DIR)/build_systems/backtrack_level/hello.c

include $(TESTS_DIR)/clang.make
