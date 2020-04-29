# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

CLANG_OPTIONS = -c
INFER_OPTIONS = \
  --biabduction-only --report-custom-error --developer-mode --no-filtering --debug-exceptions \
  --project-root $(TESTS_DIR) \

# we need to disable traces in C tests because assertions/assertion_failure.c has different traces
# on Linux and OSX because of different implementations of assert() on each platform
INFERPRINT_OPTIONS = --issues-tests-fields "file,procedure,line_offset,bug_type,bucket" --issues-tests

SOURCES = $(wildcard */*.c)

include $(TESTS_DIR)/clang.make
