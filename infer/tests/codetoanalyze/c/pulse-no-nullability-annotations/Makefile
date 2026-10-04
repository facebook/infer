# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

# The nullability annotation tests of c/pulse without --pulse-nullability-annotations

CLANG_OPTIONS = -c
INFER_OPTIONS = \
  --pulse-only --debug-exceptions --project-root $(TESTS_DIR) \
  --pulse-report-issues-for-tests --no-pulse-force-continue \
  --jobs 1 \

INFERPRINT_OPTIONS = --issues-tests

SOURCES = ../pulse/nullable_return.c ../pulse/strsep.c

include $(TESTS_DIR)/clang.make
