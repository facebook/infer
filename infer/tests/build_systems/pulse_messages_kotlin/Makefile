# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

INFER_OPTIONS = --pulse-only --debug-exceptions --no-pulse-force-continue --project-root $(TESTS_DIR)

# test that the error messages are good; sort of mimicks what gets displayed on the console
INFERPRINT_OPTIONS = --issues-tests-fields file,procedure,line,column,severity,bug_type,qualifier,suggestion --issues-tests

JAVA_SOURCES = $(wildcard *.java)
KOTLIN_SOURCES = $(wildcard *.kt)

CLEAN_EXTRA = *.class META-INF

include $(TESTS_DIR)/kotlinc_with_java.make

infer-out/report.json: $(MAKEFILE_LIST)
