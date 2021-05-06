# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../../..

INFER_OPTIONS = --seconds-per-iteration 100 --symops-per-iteration 100000 --topl-properties SkipAfterRemove.topl --topl-biabd-only
#INFER_OPTIONS = --topl-properties SkipAfterRemove.topl --topl-only
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.java)

include $(TESTS_DIR)/javac.make
