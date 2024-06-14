# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

CLANG_OPTIONS = -c $(OBJC_CLANG_OPTIONS) -fobjc-arc
INFER_OPTIONS = \
  --pulse-only --debug-exceptions --no-pulse-force-continue --project-root $(TESTS_DIR) \
  --pulse-report-issues-for-tests --pulse-inline-global-init-func-pointer \
  --report-suppress-errors TAINT_ERROR \
  --pulse-taint-config ../.infertaintconfig --pulse-taint-config .infertaintconfig

INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.m) $(wildcard */*.m)

include $(TESTS_DIR)/clang.make
include $(TESTS_DIR)/objc.make

infer-out/report.json: $(MAKEFILE_LIST)
