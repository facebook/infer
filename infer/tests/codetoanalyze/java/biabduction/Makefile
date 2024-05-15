# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

INFER_OPTIONS = --biabduction-only --debug-exceptions --biabduction-seconds-per-iteration 200
INFERPRINT_OPTIONS = --issues-tests
TEST_CLASSPATH = $(JAVA_BUILTINS_DIR)
SOURCES = $(wildcard *.java)

include $(TESTS_DIR)/javac.make

infer-out/report.json: .inferconfig
