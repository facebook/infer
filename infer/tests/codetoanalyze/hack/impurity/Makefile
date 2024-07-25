# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

INFER_OPTIONS = --impurity-only --debug-exceptions --no-pulse-force-continue

INFERPRINT_OPTIONS = --issues-tests

HH_SOURCES = $(sort $(wildcard hh/*.hack))
SOURCES = $(sort $(wildcard *.hack))

include $(TESTS_DIR)/hack.make
