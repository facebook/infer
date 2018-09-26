# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

INFER_OPTIONS = \
  --eradicate-only \
  --eradicate-return-over-annotated \
  --eradicate-optional-present \
  --eradicate-condition-redundant \
  --debug-exceptions
INFERPRINT_OPTIONS = --issues-tests
SOURCES = $(wildcard *.java) $(wildcard $(TESTS_DIR)/external/library/*.java)

include $(TESTS_DIR)/javac.make
