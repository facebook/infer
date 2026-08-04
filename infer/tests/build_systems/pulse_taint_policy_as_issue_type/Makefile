# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..
INFER_OPTIONS = --pulse-only --debug-exceptions --pulse-report-issues-for-tests --pulse-taint-policy-as-issue-type
INFERPRINT_OPTIONS = --issues-tests-fields file,procedure,line,column,severity,bug_type,qualifier --issues-tests
SOURCES = $(wildcard *.java)

include $(TESTS_DIR)/javac.make
