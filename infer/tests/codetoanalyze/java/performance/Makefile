# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

INFER_OPTIONS = --cost-only --debug-exceptions --no-cost-suppress-func-ptr
INFERPRINT_OPTIONS = --issues-tests
INFERPRINT_COST_OPTIONS = --cost-issues-tests
SOURCES = $(wildcard *.java)

include $(TESTS_DIR)/javac.make
include $(TESTS_DIR)/cost.make
