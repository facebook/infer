# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

ANALYZER = checkers
INFER_OPTIONS = \
  --impurity-only --impurity-report-immutable-modifications --report-force-relative-path \
  --no-pulse-force-continue --pulse-max-disjuncts 30 \
  --disable-issue-type IMPURE_FUNCTION \
  --disable-issue-type NULLPTR_DEREFERENCE

INFERPRINT_OPTIONS = --issues-tests
SOURCES = $(wildcard *.java)

include $(TESTS_DIR)/javac.make
