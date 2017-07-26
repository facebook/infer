# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../../..

ANALYZER = infer
CLANG_OPTIONS = -c
INFER_OPTIONS = --report-custom-error --developer-mode --no-filtering --debug-exceptions --project-root $(TESTS_DIR) --no-keep-going
# we need to disable traces in C tests because assertions/assertion_failure.c has different traces
# on Linux and OSX because of different implementations of assert() on each platform
INFERPRINT_OPTIONS = --issues-fields "file,procedure,line_offset,bug_type" --issues-tests

SOURCES = \
  $(wildcard */*.c) \

include $(TESTS_DIR)/clang.make
