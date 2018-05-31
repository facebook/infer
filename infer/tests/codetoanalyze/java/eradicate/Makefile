# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

ANALYZER = checkers
INFER_OPTIONS = \
  --eradicate-only \
	--eradicate-return-over-annotated \
	--eradicate-optional-present \
	--eradicate-condition-redundant \
	--debug-exceptions
INFERPRINT_OPTIONS = --issues-tests
SOURCES = $(wildcard *.java)

include $(TESTS_DIR)/javac.make
