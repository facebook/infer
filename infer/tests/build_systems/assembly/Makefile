# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

CLANG_OPTIONS = -c
INFER_OPTIONS = --report-custom-error --developer-mode --project-root ../codetoanalyze --Xclang -U --Xclang __SSE2__
INFERPRINT_OPTIONS = --issues-tests

SOURCES = \
  ../codetoanalyze/example.S \
  ../codetoanalyze/hello.c \

include $(TESTS_DIR)/clang.make
