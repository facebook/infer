# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

# The nullability annotation tests of cpp/pulse without --pulse-nullability-annotations

CLANG_OPTIONS = -x c++ -std=c++17 -c
INFER_OPTIONS = \
  --pulse-only --debug-exceptions --project-root $(TESTS_DIR) \
  --pulse-report-issues-for-tests --no-pulse-force-continue \

INFERPRINT_OPTIONS = --issues-tests

SOURCES = ../pulse/nonnull_annotation.cpp

include $(TESTS_DIR)/clang.make
