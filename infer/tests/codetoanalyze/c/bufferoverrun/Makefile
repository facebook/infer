# Copyright (c) 2016 - present
#
# Programming Research Laboratory (ROPAS)
# Seoul National University, Korea
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../../..

ANALYZER = checkers
CLANG_OPTIONS = -c
INFER_OPTIONS = -F --project-root $(TESTS_DIR) --no-failures-allowed --no-liveness --bufferoverrun
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.c)

include $(TESTS_DIR)/clang.make
