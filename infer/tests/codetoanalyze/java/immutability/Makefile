# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

ANALYZER = checkers
INFER_OPTIONS = \
  --impurity-only --no-pulse-force-continue --impurity-report-immutable-modifications \
  --disable-issue-type IMPURE_FUNCTION
INFERPRINT_OPTIONS = --issues-tests
SOURCES = $(wildcard *.java)

include $(TESTS_DIR)/javac.make
