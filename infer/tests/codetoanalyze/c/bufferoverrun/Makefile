# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../../..

ANALYZER = bufferoverrun
# see explanations in cpp/errors/Makefile for the custom isystem
CLANG_OPTIONS = -c
INFER_OPTIONS = --bo-filtering 0 --project-root $(TESTS_DIR)
INFERPRINT_OPTIONS = --issues-tests

SOURCES = \
  test1.c

include $(TESTS_DIR)/clang.make
