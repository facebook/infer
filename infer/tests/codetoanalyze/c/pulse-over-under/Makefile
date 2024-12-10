# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

# Tests for the over-approximating pulse domain mixed with the disjunctive domain

CLANG_OPTIONS = -c
INFER_OPTIONS = \
  --pulse-only --debug-exceptions --project-root $(TESTS_DIR) \
  --pulse-report-issues-for-tests --no-pulse-force-continue \
  --pulse-max-disjuncts 1 --pulse-over-approximate-reasoning \

INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.c */*.c)

include $(TESTS_DIR)/clang.make
