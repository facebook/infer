# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

CLANG_OPTIONS = -c $(OBJC_CLANG_OPTIONS) -fobjc-arc
INFER_OPTIONS = --pulse-only --debug-exceptions --project-root $(TESTS_DIR) \
--pulse-report-issues-for-tests --inline-func-pointer-for-testing "mockptr_" \
--function-pointer-specialization \
--report-suppress-errors SENSITIVE_DATA_FLOW --report-suppress-errors DATA_FLOW_TO_SINK \
--pulse-taint-config ../.infertaintconfig --pulse-taint-config taint/.infertaintconfig \
--dynamic-dispatch-json-file-path taint/dispatch.json \
--pulse-model-return-first-arg "release\\|.*release:\\|release_fst\\|ModelClass.bridge:" \
--pulse-model-return-this "ModelClass.initWith:"

INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.m) $(wildcard */*.m)

include $(TESTS_DIR)/clang.make
include $(TESTS_DIR)/objc.make

infer-out/report.json: $(MAKEFILE_LIST)
