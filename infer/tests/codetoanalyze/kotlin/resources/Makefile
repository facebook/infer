# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

SOURCES = $(wildcard *.kt)
CLEAN_EXTRA = *.class META-INF
INFERPRINT_OPTIONS = --issues-tests
INFER_OPTIONS = --pulse-only --debug-exceptions --no-pulse-force-continue

include $(TESTS_DIR)/kotlinc.make
