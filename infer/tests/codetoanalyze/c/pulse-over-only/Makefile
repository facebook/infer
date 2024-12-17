# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

# Tests for the over-approximating pulse domain on its own

CLANG_OPTIONS = -c
INFER_OPTIONS = \
  --pulse-only --debug-exceptions --project-root $(TESTS_DIR) \
  --pulse-report-issues-for-tests --no-pulse-force-continue \
  --pulse-max-disjuncts 0 --pulse-over-approximate-reasoning \
  --pulse-transitive-access-config transitive-access.conf \

INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.c */*.c)

include $(TESTS_DIR)/clang.make
