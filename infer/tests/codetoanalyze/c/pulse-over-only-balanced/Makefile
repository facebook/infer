# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

# The tests of ../pulse-over-only with the balanced disjunct strategy, which must give the same
# results since no disjunct is kept

CLANG_OPTIONS = -c
INFER_OPTIONS = \
  --pulse-only --debug-exceptions --project-root $(TESTS_DIR) \
  --pulse-report-issues-for-tests --no-pulse-force-continue \
  --pulse-max-disjuncts 0 --pulse-over-approximate-reasoning \
  --pulse-balanced-disjuncts-strategy \
  --pulse-transitive-access-config ../pulse-over-only/transitive-access.conf \

INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard ../pulse-over-only/*.c)

include $(TESTS_DIR)/clang.make
