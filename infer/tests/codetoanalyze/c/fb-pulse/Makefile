# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

CLANG_OPTIONS = -c
INFER_OPTIONS = --pulse-only --debug-exceptions --project-root $(TESTS_DIR) \
--pulse-report-issues-for-tests --function-pointer-specialization \
--report-force-relative-path

INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.c)

include $(TESTS_DIR)/clang.make

infer-out$(TEST_SUFFIX)/report.json: .inferconfig
