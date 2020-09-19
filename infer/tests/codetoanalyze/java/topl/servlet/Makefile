# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../../..

INFER_OPTIONS = --pulse-max-disjuncts 100 --topl-properties servlet.topl --topl-pulse-only
INFERPRINT_OPTIONS = --issues-tests
TEST_CLASSPATH = javax.servlet-api-4.0.1.jar

SOURCES = $(wildcard *.java)

include $(TESTS_DIR)/javac.make
