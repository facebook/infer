# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

INFER_OPTIONS = --debug-exceptions --annotation-reachability-only
INFERPRINT_OPTIONS = --issues-tests

JAVA_SOURCES = $(wildcard *.java)
KOTLIN_SOURCES = $(wildcard *.kt)

CLEAN_EXTRA = *.class META-INF

include $(TESTS_DIR)/kotlinc_with_java.make
