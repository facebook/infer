# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

CLANG_OPTIONS = -c $(OBJC_CLANG_OPTIONS) -fobjc-arc
INFER_OPTIONS = \
  --no-capture --linters-only --no-filtering --debug-exceptions --project-root $(TESTS_DIR) \
  --linters-doc-url "ASSIGN_POINTER_WARNING:www.example.com"
INFERPRINT_OPTIONS = --issues-tests

SOURCES = \
  $(wildcard *.m) \
  $(wildcard */*.m) \

include $(TESTS_DIR)/clang.make
include $(TESTS_DIR)/objc.make
