# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../../..

ANALYZER = checkers
# see explanations in cpp/errors/Makefile for the custom isystem
CLANG_OPTIONS = -x objective-c -c
INFER_OPTIONS = --uninit-only --debug-exceptions --project-root $(TESTS_DIR)
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.m)

include $(TESTS_DIR)/clang.make

infer-out/report.json: $(MAKEFILE_LIST)
