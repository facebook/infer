# Copyright (c) 2018-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

INFER_OPTIONS = --bufferoverrun-only --no-filtering --debug-exceptions \
                --project-root $(TESTS_DIR)

INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.java)

include $(TESTS_DIR)/javac.make
